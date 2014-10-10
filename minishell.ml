open Core.Std

let arg_parse line =

  let rec skip_blanks i =
    if i < String.length line && line.[i] = ' '
    then skip_blanks (i+1)
    else i
  in
  let rec split start i =
    if i >= String.length line then
      if start = i then
        []
      else
        [String.sub line start (i-start)]
    else if line.[i] = ' ' then
      let j = skip_blanks i in
      String.sub line start (i-start) :: split j j
    else
      split start (i+1)
  in
  let j = skip_blanks 0 in
  split j j

let process_line line =
  let open Unix in
  match arg_parse line with
  | [] -> failwith ""
  | prog :: _ as args ->
     match fork () with
     | `In_the_child   ->
        never_returns (exec ~prog ~args ~use_path:true ())
     | `In_the_parent cpid ->
        try
          let _ = waitpid cpid in
          ()
        with
        | Unix_error (err, _, _) ->
           Out_channel.output_string Out_channel.stderr (error_message err)

let rec prompt () =
  Out_channel.output_string stderr "% ";
  Out_channel.flush stderr;
  match (In_channel.input_line stdin) with
  | None -> ()
  | Some line ->
     process_line line;
     prompt ()

(* Launch the prompt function as main *)
let () =
  prompt ()
