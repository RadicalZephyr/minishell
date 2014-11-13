open Core.Std

let process_line line =
  let open Unix in
  let (_, args) = Arg_parse.process line in
  match args with
  | [] -> () (* silently ignore empty lines *)
  | prog :: just_args ->
     match Builtin.try_all prog just_args {in_ch  = Pervasives.stdin;
                                           out_ch = Pervasives.stdout;
                                           err_ch = Pervasives.stderr} with
     | Some exit_status ->
        Globals.Exit_status.set exit_status
     | None ->
        match fork () with
        | `In_the_child   ->
           begin
             try
               never_returns (exec ~prog ~args ~use_path:true ())
             with
             | Unix_error (err_code, component, _) ->
                (* Set status of 127 on exec failure *)
                fprintf Pervasives.stderr "%s: %s\n"
                        component
                        (error_message err_code);
                exit 127
           end

        | `In_the_parent cpid ->
           try
             begin
               match waitpid cpid with
               | Ok () ->
                  Globals.Exit_status.set 0
               | Error `Exit_non_zero exit_status ->
                  Globals.Exit_status.set exit_status
               | Error `Signal _ ->
                  ()
             end
           with
           | Unix_error (err, _, _) ->
              Out_channel.output_string Out_channel.stderr (error_message err)

let prompt ?(do_prompt=true) in_chan =
  let rec prompt_rec () =
    if do_prompt then
      Out_channel.output_string stderr "% ";
    Out_channel.flush stderr;

    match (In_channel.input_line in_chan) with
    | None -> ()
    | Some line ->
       process_line line;
       prompt_rec ()
  in
  prompt_rec ()

(* Launch the prompt function as main *)
let () =
  if (Array.length Sys.argv) = 1 then prompt stdin
  else
    begin
      (* Permanently ignore the msh argument for purposes of arguments *)
      Globals.Shell_args.set_prog_index 1;
      In_channel.with_file Sys.argv.(1) ~f:(prompt ~do_prompt:false)
    end
