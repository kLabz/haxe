if Sys.ocaml_version < "3.11"
then
  failwith "Objsize >=0.12 can only be used with OCaml >=3.11"

type info =
  { data : int
  ; headers : int
  ; depth : int
  ; reached : bool
  }

(* The original C [ml_objsize] stub walked the value graph with a seeded visited
   set (the [exclude] roots) and reported whether any [reach] root was hit. That
   stub is gone; we rebuild the same semantics on top of [Obj.reachable_words],
   which counts each distinct heap block once for a given set of roots (so it
   handles sharing and cycles). The key identity is inclusion-exclusion:

     words reachable from [obj] but NOT already reachable from [exclude]
       = reach({obj} U exclude) - reach(exclude)

   i.e. the *marginal* size [obj] adds on top of what is counted elsewhere. That
   is exactly the partition the memory report wants (no double counting of the
   shared compiler/type graph across modules or across macro-interp children). *)

(* Union of words reachable from a set of roots, counting shared blocks once.
   We pack the roots into an array and let [Obj.reachable_words] traverse it,
   then subtract the array block's own overhead (header + one slot per root). *)
let reach_set (roots : Obj.t list) =
  match roots with
  | [] -> 0
  | _ ->
    let n = List.length roots in
    let a = Array.of_list roots in
    (Obj.reachable_words (Obj.repr a)) - (n + 1)

let objsize obj (exclude:Obj.t list) (reach:Obj.t list) =
  let v = Obj.repr obj in
  (* marginal words of [v] not already accounted for by [exclude] *)
  let data = (reach_set (v :: exclude)) - (reach_set exclude) in
  (* [v] "reaches" the boundary if its graph shares any block with [reach]'s graph *)
  let reached = match reach with
    | [] -> false
    | _ ->
      let only_v = reach_set [v] in
      let only_reach = reach_set reach in
      let both = reach_set (v :: reach) in
      both < only_v + only_reach
  in
  { data; headers = 0; depth = 0; reached }

let size_with_headers i = (Sys.word_size/8) * (i.data + i.headers)

let size_without_headers i = (Sys.word_size/8) * i.data

let reachable_bytes_of roots = (Sys.word_size/8) * (reach_set roots)
