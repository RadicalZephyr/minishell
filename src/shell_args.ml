open Core.Std

type t =
  { argc : int;
    argv : bytes array;
    mutable prog_index : int;
    mutable offset : int;
  }

let args =
  { argc = Array.length Sys.argv;
    argv = Sys.argv;
    prog_index = 0;
    offset = 0 }

(** For all intents and purposes this represents an unalterable
    shift of the arguments *)
let set_prog_index n =
  args.prog_index <- n

let shift n =
  args.offset <- args.offset + n

let unshift n =
  args.offset <- args.offset - n

let clear () =
  args.offset <- 0

let count () =
  args.argc - args.offset - args.prog_index

let get index =
  let shifted_len = count () in
  let shifted_index = index + args.offset + args.prog_index in
  if index >= shifted_len then ""
  else args.argv.(shifted_index)
