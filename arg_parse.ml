open Core.Std

let process line =

  let print_info marker accum buffer start i j =
    match accum with
    | [] ->
       printf "[%s] buf: '%s' start: %d i: %d j:%d\n%!"
              marker (Buffer.contents buffer) start i j
    | hd :: _ ->
       printf "[%s] hd: '%s' buf: '%s' start: %d i: %d j:%d\n%!"
              marker hd (Buffer.contents buffer) start i j
  in

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
       Some ('}', (fun (str) -> match Sys.getenv str with
                                | None -> ""
                                | Some var -> var))
    | _ -> None
  in

  let replace_with_substring_from_index_to_char fn start end_char =
    let endi = find_char start end_char in
    let replacement = fn (String.sub line ~pos:start ~len:((endi-1)-start)) in
    (endi, replacement)
  in

  let rec split accum buffer start i =
    let append_to_buffer first last =
      if last >= first then
        Buffer.add_substring buffer line first (last-first)
      else ()
    in

    if i >= String.length line then begin
        append_to_buffer start i;
        let arg = Buffer.contents buffer in
        if String.length arg = 0 then accum
        else arg :: accum
      end
    else
      match line.[i] with
      | ' ' ->
         let j = skip_blanks i in

         append_to_buffer start i;
         split ((Buffer.contents buffer) :: accum) (Buffer.create 10) j j

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

           | Some (char, fn) ->
              let (endi, replacement) =
                replace_with_substring_from_index_to_char fn (i+2) char in
              append_to_buffer start (i-1);
              Buffer.add_string buffer replacement;
              let next = (endi+1) in
              split accum buffer next next
         end
      | _ ->
         split accum buffer start (i+1)
  in

  let j = skip_blanks 0 in
  let args = split [] (Buffer.create 10) j j in
  (List.length args, List.rev args)
