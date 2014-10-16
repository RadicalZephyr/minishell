open Core.Std

type t =
  { rev_arg_list: bytes list;
    current_arg : Buffer.t;
    start       : int;
    i           : int;
  }

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

  let rec find_quote i =
    if i > String.length line then failwith "No ending quote found."
    else if line.[i] = '"' then i
    else find_quote (i+1)
  in

  let rec split accum buffer start i =
    if i >= String.length line then begin
        Buffer.add_substring buffer line start (i-start);
        let arg = Buffer.contents buffer in
        if String.length arg = 0 then accum
        else arg :: accum
      end
    else
      match line.[i] with
      | ' ' ->
         let j = skip_blanks i in

         Buffer.add_substring buffer line start (i-start);
         split ((Buffer.contents buffer) :: accum) (Buffer.create 10) j j

      | '"' ->
         let j = find_quote (i+1) in
         (* Copy the arg up to the starting quote *)
         Buffer.add_substring buffer line start (i-start);

         (* Copy the characters between the quotations *)
         Buffer.add_substring buffer line (i+1) ((j-1)-i);

         (* ... and reset the start pointer to after both strings *)
         split accum buffer (j+1) (j+1)

      | _ ->
         split accum buffer start (i+1)
  in

  let j = skip_blanks 0 in
  let args = split [] (Buffer.create 10) j j in
  (List.length args, List.rev args)
