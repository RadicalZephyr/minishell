open Core.Std

type t =
  | Not_a_builtin
  | Ok of int

type builtin_rec = {
    name : bytes;
    fn   : bytes list -> int;
  }


let try_all prog args =
  let exit_fn args =
    match args with
    | [] -> exit 0
    | hd :: _ -> exit (int_of_string hd)
  in

  let aecho args =
    let rec build_string accum args =
      match args with
      | [] -> Buffer.contents accum
      | hd :: tl ->
         Buffer.add_char accum ' ';
         Buffer.add_string accum hd;
         build_string accum tl
    in
    match args with
    | [] -> printf "\n"; 0
    | hd :: tl ->
       let terminal = if hd = "-n" then "\n" else "" in
       printf "%s%s%s" hd (build_string (Buffer.create 15) tl) terminal; 0
  in

  let builtins = [{ name = "exit";  fn = exit_fn};
                  { name = "aecho"; fn = aecho}]
  in
  List.find_map builtins ~f:(fun ({name; fn}) ->
                             if prog = name then Some (fn args)
                             else None)
