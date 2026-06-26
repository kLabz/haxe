(* Information gathered while walking through values. *)
type info =
  { data : int
  ; headers : int
  ; depth : int
  ; reached : bool
  }

(* Returns information for first argument, excluding the second arg list and telling if we can reach the third arg list *)
val objsize : 'a -> Obj.t list -> Obj.t list -> info

(* Calculates sizes in bytes: *)
val size_with_headers : info -> int
val size_without_headers : info -> int

(* Bytes reachable from the union of the given roots, counting each shared block
   once. Useful for building a partition via prefix sums:
   size(root_i) = reachable_bytes_of (root_i :: prefix) - reachable_bytes_of prefix *)
val reachable_bytes_of : Obj.t list -> int
