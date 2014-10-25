(** Shift all arguments n indices to the left *)
val shift : int -> unit

(** Shift all arguments n indices to the right *)
val unshift : int -> unit

(** Get the current count of arguments, respecting shifts *)
val count : unit -> int

(** Retrieve a shell argument at the specified
    index, respecting shifts *)
val get : int -> bytes
