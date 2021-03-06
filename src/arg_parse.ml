open Core.Std

type skipf =
  | Skip_first of int

type skipl =
  | Skip_last of int

let process line =

  let rec skip_blanks i =
    if i < String.length line && line.[i] = ' ' then skip_blanks (i+1)
    else i
  in

  let find_char i char =
    String.index_from_exn line i char
  in

  let get_replacement_for char =
    match char with
    | '{' ->
       Some ((Char.equal '}'), Skip_first 1, Skip_last 1,
             (fun (str) -> match Sys.getenv str with
                           | None -> ""
                           | Some var -> var), true)
    | '$' ->
       Some ((Char.equal '$'), Skip_first 0, Skip_last 1,
             (fun (_) -> Pid.to_string (Unix.getpid ())), false)
    | '#' ->
       Some ((Char.equal '#'), Skip_first 0, Skip_last 1,
             (fun (_) -> Int.to_string (Globals.Shell_args.count ())), false)
    | '?' ->
       Some ((Char.equal '?'), Skip_first 0, Skip_last 1,
            (fun (_) -> Int.to_string (Globals.Exit_status.get ())), false)
    | '0' .. '9' ->
       Some ((fun (c) -> not (Char.is_digit c)), Skip_first 0, Skip_last 0,
             (fun (num) -> Globals.Shell_args.get (Int.of_string num)), false)
    | _ -> None
  in

  let char_equal_in_line fn _ char =
    fn char
  in

  let replace_with_substring_from_index_to_char fn start char_match should_error =
    let replacement_of_index index =
      let replacement = fn (String.sub line ~pos:start ~len:(index-start)) in
      (index, replacement)
    in

    match String.lfindi ~pos:start line ~f:char_match with
    | None ->
       if should_error then
         failwith "Could not find an ending character for match."
       else
         replacement_of_index (String.length line)
    | Some endi ->
       replacement_of_index endi
  in

  let rec split accum buffer start i =
    let append_to_buffer first last =
      if last >= first && (String.length line) >= last then
        Buffer.add_substring buffer line first (last-first)
      else ()
    in

    let append_arg_from_buffer accum buffer =
      if Buffer.length buffer = 0 then accum
      else (Buffer.contents buffer) :: accum
    in

    if i >= String.length line then begin
        append_to_buffer start i;
        append_arg_from_buffer accum buffer
      end
    else
      match line.[i] with
      | ' ' ->
         let j = skip_blanks i in

         append_to_buffer start i;
         split (append_arg_from_buffer accum buffer) (Buffer.create 10) j j

      | '"' ->
         let j = find_char (i+1) '"' in
         (* Copy the arg up to the starting quote *)
         append_to_buffer start i;

         (* Copy the characters between the quotations *)
         append_to_buffer (i+1) j;

         (* ... and reset the start pointer to after both strings *)
         split accum buffer (j+1) (j+1)

      (* Process expansions *)
      | '$' ->
         begin
           match get_replacement_for (line.[i+1]) with
           | None ->
              split accum buffer start (i+1)

           (* Search for char in the line after current point, then
                    pass the resulting substring, skipping the first n
                    characters, to the function fn *)
           | Some (char_match, Skip_first n, Skip_last m, fn, should_error) ->
              let (endi, replacement) =
                replace_with_substring_from_index_to_char
                  fn (i+n+1) (char_equal_in_line char_match) should_error
              in
              append_to_buffer start (i-1);
              Buffer.add_string buffer replacement;
              let next = (endi+m) in
              split accum buffer next next
         end
      | _ ->
         split accum buffer start (i+1)
  in

  let j = skip_blanks 0 in
  let args = split [] (Buffer.create 10) j j in
  (List.length args, List.rev args)
