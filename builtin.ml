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
    1
  in

  let aecho args =
    1
  in

  let builtins = [{ name = "exit";  fn = exit_fn};
                  { name = "aecho"; fn = aecho}]
  in
  List.find_map builtins ~f:(fun ({name; fn}) ->
                             if prog = name then Some (fn args)
                             else None)
