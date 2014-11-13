open Core.Std

module Exit_status : sig

    val set : int -> unit

    val get : unit -> int

  end = struct

    type t =
      {
        mutable last : int;
      }

    let exit_status = { last = 0 }

    let set n =
      exit_status.last <- n

    let get () =
      exit_status.last

  end


module Shell_args : sig
    (** Set the basic unalterable offset of the args *)
    val set_prog_index : int -> unit

    (** Shift all arguments n indices to the left *)
    val shift : int -> unit

    (** Shift all arguments n indices to the right *)
    val unshift : int -> unit

    (** Clear present shift completely *)
    val clear : unit -> unit

    (** Get the current count of arguments, respecting shifts *)
    val count : unit -> int

    (** Retrieve a shell argument at the specified
    index, respecting shifts *)
    val get : int -> bytes

  end = struct

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
  end
