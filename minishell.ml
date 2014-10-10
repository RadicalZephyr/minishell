open Core.Std


let process_line line =
  ()


let prepare_line line =
  ()

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
