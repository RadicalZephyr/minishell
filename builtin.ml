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
    | [] -> printf "\n"; 0
    | "-n" :: [] -> 0
    | "-n" :: args  -> print args ""  ; 0
    | args          -> print args "\n"; 0
  in

  let envset args =
    match args with
    (* zero or one arguments is an error *)
    | [] | _ :: [] -> failwith "too few arguments"
    | key :: data :: _ ->
       Unix.putenv ~key ~data;
       0
  in

  let envunset args =
    match args with
      (* zero arguments is an error *)
    | [] -> failwith "too few arguments"
    | name :: _ ->
       Unix.unsetenv name;
       0
  in


  let builtins = [{ name = "exit";     fn = exit_fn};
                  { name = "aecho";    fn = aecho};
                  { name = "envset";   fn = envset};
                  { name = "envunset"; fn = envunset}]
  in
  match args with
  | [] -> assert false
  | _ :: args ->
     List.find_map builtins ~f:(fun ({name; fn}) ->
                                if prog = name then Some (fn args)
                                else None)
