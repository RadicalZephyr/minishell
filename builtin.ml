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

  let shift args =
    match args with
    | [] -> Shell_args.shift 1; 0
    | nstr :: _ ->
       begin
         match Option.try_with (fun () -> Int.of_string nstr) with
         | None -> failwith (sprintf "shift: '%s' is not a number" nstr)
         | Some n -> Shell_args.shift n; 0
       end
  in

  let unshift args =
    match args with
    | [] -> Shell_args.clear (); 0
    | nstr :: _ ->
       begin
         match Option.try_with (fun () -> Int.of_string nstr) with
         | None -> failwith (sprintf "unshift: '%s' is not a number" nstr)
         | Some n -> Shell_args.unshift n; 0
       end

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
