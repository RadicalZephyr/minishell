open Core.Std

type t =
  { argc : int;
    argv : bytes array;
    mutable offset : int;
  }

let args =
  { argc = Array.length Sys.argv;
    argv = Sys.argv;
    offset = 0}


let shift n =
  args.offset <- args.offset + n

let unshift n =
  args.offset <- args.offset - n

let get index =
  let shifted_len = args.argc - args.offset in
  let shifted_index = index + args.offset in
  if index > shifted_len then ""
  else args.argv.(shifted_index)
