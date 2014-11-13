open Core.Std

type t =
  | Not_a_builtin
  | Ok of int

type builtin_rec = {
    name : bytes;
    fn   : bytes list -> int;
  }

type file_channels = {
    in_ch :  in_channel;
    out_ch : out_channel;
    err_ch : out_channel;
  }

let try_all prog args channels =
  let exit_fn args =
    match args with
    | [] -> exit 0
    | hd :: _ -> exit (int_of_string hd)
  in

  let aecho args =
    let print hd terminal =
      fprintf channels.out_ch "%s%s%!"
              (String.concat hd ~sep:" ") terminal
    in

    match args with
    | "-n" :: [] -> 0
    | "-n" :: args  -> print args ""  ; 0
    | args          -> print args "\n"; 0
  in

  let envset args =
    match args with
    (* zero or one arguments is an error *)
    | [] | _ :: [] -> failwith "envset: too few arguments"
    | key :: data :: _ ->
       Unix.putenv ~key ~data;
       0
  in

  let envunset args =
    match args with
      (* zero arguments is an error *)
    | [] -> failwith "envunset: too few arguments"
    | name :: _ ->
       Unix.unsetenv name;
       0
  in

  let cd args =
    let cd_to dir =
      match Sys.is_directory dir with
      | `No | `Unknown -> failwith (sprintf "cd: %s is not a directory" dir)
      | `Yes -> Sys.chdir dir
    in
    match args with
    | [] ->
       begin
         match Sys.getenv "HOME" with
         | None -> failwith "cd: HOME is not set in the environment"
         | Some home -> cd_to home; 0
       end
    | dir :: _ -> cd_to dir; 0
  in

  let with_num_arg default_fn n_fn name =
    begin
      fun (args) ->
      match args with
      | [] -> default_fn (); 0
      | nstr :: _ ->
         begin
           match Option.try_with (fun () -> Int.of_string nstr) with
           | None -> failwith (sprintf "%s: '%s' is not a number" name nstr)
           | Some n -> n_fn n; 0
         end
    end
  in

  let shift =
    with_num_arg (fun () -> Globals.Shell_args.shift 1) Globals.Shell_args.shift "shift"
  in

  let unshift =
    with_num_arg (fun () -> Globals.Shell_args.clear ()) Globals.Shell_args.unshift "unshift"
  in

  let sstat args =
    let stat_file fname =
      try
        let stats = Unix.stat fname in
        fprintf channels.out_ch "%s %s %s %s %s %s\n"
                fname
                (Get_info.uid_name stats.st_uid)
                (Get_info.gid_name stats.st_gid)
        (* I need getgrgid and getpwuid *)
        0
      with
      | Unix.Unix_error (err_code, component, _) ->
         fprintf channels.stderr "%s: %s\n"
                 component
                 (error_message err_code);
         1
    in

    let itr args =
      match args with
      | [] -> 0
      | hd :: tl ->
         stat_file hd;
         itr tl
    in

    match args with
    | [] ->
       fprintf channels.stderr "stat: missing operand\n";
       1
    | _ ->
       itr args
  in

  let builtins = [{ name = "exit"    ; fn = exit_fn };
                  { name = "aecho"   ; fn = aecho   };
                  { name = "envset"  ; fn = envset  };
                  { name = "envunset"; fn = envunset};
                  { name = "cd"      ; fn = cd      };
                  { name = "shift"   ; fn = shift   };
                  { name = "unshift" ; fn = unshift }]
  in
  List.find_map builtins ~f:(fun ({name; fn}) ->
                             if prog = name then Some (fn args)
                             else None)
