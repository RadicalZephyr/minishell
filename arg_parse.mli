open Core.Std

(** Process an input line into a count, and a list of strings *)
val process : bytes -> int * bytes list
