open Core.Std

let process line =

  let rec skip_blanks i =
    if i < String.length line && line.[i] = ' '
    then skip_blanks (i+1)
    else i
  in

  let rec split accum start i =
    if i >= String.length line then

      if start = i then
        accum
      else
        String.sub line start (i-start) :: accum

    else if line.[i] = ' ' then

      let j = skip_blanks i in
      split (String.sub line start (i-start) :: accum) j j

    else
      split accum start (i+1)
  in

  let j = skip_blanks 0 in
  List.rev (split [] j j)
