open Core.Std

let process_line line =
  let open Unix in
  match Arg_parse.process line with
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
