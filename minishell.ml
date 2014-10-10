open Core.Std
open Unix

let process_line line =
  match fork () with
  | `In_the_child   ->
     never_returns (exec ~prog:line ~args:[line] ~use_path:true ())
  | `In_the_parent cpid ->
     try
       let _ = waitpid cpid in
       ()
     with
     | Unix_error (err, _, _) ->
        Out_channel.output_string Out_channel.stderr (error_message err)

let rec prompt () =
  Out_channel.output_string Out_channel.stderr "% ";
  Out_channel.flush Out_channel.stderr;
  match (In_channel.input_line In_channel.stdin) with
  | None -> ()
  | Some line ->
     process_line line;
     prompt ()

(* Launch the prompt function as main *)
let () =
  prompt ()
