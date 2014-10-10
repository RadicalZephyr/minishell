open Core.Std

let arg_parse line =
  [line]

let process_line line =
  let open Unix in
  let args = arg_parse line in
  match fork () with
  | `In_the_child   ->
     never_returns (exec ~prog:line ~args:args ~use_path:true ())
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
