(** Shift all arguments n indices to the left *)
val shift : int -> unit

(** Shift all arguments n indices to the right *)
val unshift : int -> unit

(** Retrieve a shell argument at the specified
    index, respecting shifts *)
val get : int -> bytes
