open Core.Std

let process line =

  let rec skip_blanks i =
    if i < String.length line && line.[i] = ' '
    then skip_blanks (i+1)
    else i
  in

  let rec find_quote i =
    if line.[i] = '"'
    then i
    else if i < String.length line && line.[i] <> '"'
    then failwith "No ending quote found."
    else find_quote (i+1)
  in

  let rec split accum counter start i =
    if i >= String.length line then

      if start = i then
        (counter, accum)
      else
        (counter+1, String.sub line start (i-start) :: accum)

    else
      match line.[i] with
      | ' ' ->
         let j = skip_blanks i in
         split (String.sub line start (i-start) :: accum) (counter+1) j j

      | '"' ->
         let j = find_quote i+1 in
         split accum counter start (j+1)

      | _ ->
         split accum counter start (i+1)
  in

  let j = skip_blanks 0 in
  let (arg_count, args) = split [] 0 j j in
  (arg_count, List.rev args)
