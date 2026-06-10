(*
	Module headers — Phase 1.

	A [module_header] is a first-class, structured snapshot of a module's typed *signature*
	(no expression bodies), generated post-typing so that inferred and macro-generated field
	types are captured accurately. It is the unit dependencies (will) point at and that
	invalidation diffs.

	Phase 1 deliberately makes NO [Type.t] change: references still point at the full [tclass].
	The header is generated and diffed, not yet a link target. Each signature leaf is reduced to
	a canonical, deterministic *string* (faithful to everything a dependent can observe), so two
	headers — including one deserialized from a previous session — can be compared field by field.

	The header object is structured (per type decl, per field) so it carries forward into Phase 2
	unchanged; only the leaf representation (string today, lazy link later) is expected to evolve.
*)

open Globals
open Ast
open TType
open TFunctions

(* The [module_header] / [header_decl] / [header_entry] types live in TType so that
   [module_def_extra] can hold one ([m_header]). Field keys never collide: "i:" member,
   "s:" static, "c:" constructor, "e:" enum constructor. *)

(* ---------------------------------------------------------------------- *)
(* Canonical signature printing                                            *)

(* A faithful, deterministic rendering of a type. Unlike [s_type_kind] this fully expands
   structural types (anons, function arguments incl. optionality) so that any observable change
   is detected; unlike [s_type] it never follows typedefs/abstracts (their nominal identity is
   observable) and it forces lazies + bound monos so the result is stable. *)
let rec s_sig t =
	match t with
	| TMono r ->
		begin match r.tm_type with
			| Some t -> s_sig t
			| None -> "?"
		end
	| TLazy f ->
		s_sig (lazy_type f)
	| TEnum(en,tl) -> Printf.sprintf "E#%s%s" (s_type_path en.e_path) (s_params tl)
	| TInst(c,tl) ->
		begin match c.cl_kind with
		| KTypeParameter ttp -> Printf.sprintf "T#%s%s" ttp.ttp_name (s_params tl)
		| _ -> Printf.sprintf "C#%s%s" (s_type_path c.cl_path) (s_params tl)
		end
	| TType(td,tl) -> Printf.sprintf "D#%s%s" (s_type_path td.t_path) (s_params tl)
	| TAbstract(a,tl) -> Printf.sprintf "A#%s%s" (s_type_path a.a_path) (s_params tl)
	| TFun(args,ret) ->
		let s_arg (n,o,t) = Printf.sprintf "%s%s:%s" (if o then "?" else "") n (s_sig t) in
		Printf.sprintf "(%s)->%s" (String.concat "," (List.map s_arg args)) (s_sig ret)
	| TAnon an ->
		(* Sort fields by name for determinism; include each field's kind so property
		   accessors and method/var distinctions are observable. *)
		let fields = PMap.fold (fun cf acc -> cf :: acc) an.a_fields [] in
		let fields = List.sort (fun a b -> compare a.cf_name b.cf_name) fields in
		let s_field cf = Printf.sprintf "%s%s:%s" (s_field_kind cf.cf_kind) cf.cf_name (s_sig cf.cf_type) in
		Printf.sprintf "{%s}" (String.concat "," (List.map s_field fields))
	| TDynamic None -> "Dynamic"
	| TDynamic (Some t) -> Printf.sprintf "Dynamic<%s>" (s_sig t)

and s_params = function
	| [] -> ""
	| tl -> Printf.sprintf "<%s>" (String.concat "," (List.map s_sig tl))

and s_field_kind = function
	| Method MethNormal -> ""
	| Method MethInline -> "inline "
	| Method MethDynamic -> "dynamic "
	| Method MethMacro -> "macro "
	| Var { v_read = r; v_write = w } -> Printf.sprintf "(%s,%s)" (s_var_access r) (s_var_access w)

and s_var_access = function
	| AccNormal -> "default"
	| AccNo -> "null"
	| AccNever -> "never"
	| AccCtor -> "ctor"
	| AccCall -> "get/set"
	| AccPrivateCall -> "get/set(priv)"
	| AccInline -> "inline"
	| AccRequire(s,_) -> "require(" ^ s ^ ")"

(* Type parameters: name, constraints and default all affect what a dependent may pass. *)
let s_type_params params =
	let s_ttp ttp =
		let constraints = match ttp.ttp_constraints with
			| None -> ""
			| Some lz ->
				begin match AtomicLazy.force lz with
				| [] -> ""
				| tl -> ":" ^ String.concat "&" (List.map s_sig tl)
				end
		in
		let def = match ttp.ttp_default with None -> "" | Some t -> "=" ^ s_sig t in
		ttp.ttp_name ^ constraints ^ def
	in
	match params with
	| [] -> ""
	| _ -> "<" ^ String.concat "," (List.map s_ttp params) ^ ">"

(* ---------------------------------------------------------------------- *)
(* Meta                                                                    *)

(* Meta can be signature-affecting (@:op, @:from, @:native, @:overload, ...). We render it
   faithfully and position-free via the AST printer rather than maintaining a fragile allowlist:
   over-rendering only costs precision (a spurious diff), never soundness. Internal/noise meta
   that is unstable across runs would cause spurious diffs, but headers are generated post-typing
   on both sides so the same meta appears each time. Sorted for determinism. *)
(* Whole-program analysis markers (added by DCE etc.) are NOT part of a module's signature:
   they depend on unrelated code, so including them would make the header churn non-locally.
   Headers are normally generated pre-DCE, but filter defensively so the dump tool agrees. *)
let is_internal_meta (m,_,_) = match m with
	| Meta.Used | Meta.DirectlyUsed -> true
	| _ -> false

let s_meta meta =
	let meta = List.filter (fun m -> not (is_internal_meta m)) meta in
	match meta with
	| [] -> ""
	| _ ->
		let entries = List.map (fun m -> Ast.Printer.s_metadata "" m) meta in
		String.concat " " (List.sort compare entries)

(* ---------------------------------------------------------------------- *)
(* Field signatures                                                        *)

(* Observable, post-typing-stable class-field flags. DCE markers (CfUsed/CfMaybeUsed),
   CfPostProcessed and CfModifiesThis (an implementation property, relevant only to inliners)
   are excluded so they don't churn the header. *)
let observable_field_flags = [
	CfPublic; CfStatic; CfExtern; CfFinal; CfOverride; CfAbstract; CfOverload;
	CfImpl; CfEnum; CfGeneric; CfDefault; CfNoLookup; CfAbstractConstructor;
]

let s_field_flags cf =
	let l = List.filter (fun flag -> has_class_field_flag cf flag) observable_field_flags in
	String.concat "," (List.map (fun f -> List.nth flag_tclass_field_names (int_of_class_field_flag f)) l)

(* Implementation fields — inline / macro / @:generic — carry their *body* into callers
   (inlining, specialization, compile-time eval), so the body is part of what a dependent observes.
   For these (and only these) the header includes a canonical rendering of [cf_expr], so a body
   change is visible to [header_diff] even when the signature is unchanged. *)
let is_impl_field cf =
	match cf.cf_kind with
	| Method (MethInline | MethMacro) -> true
	| Var { v_read = AccInline } -> true
	| _ -> has_class_field_flag cf CfGeneric

let s_field_body cf =
	if is_impl_field cf then
		match cf.cf_expr with
		| Some e -> TPrinting.s_expr_pretty false "" false s_sig e
		| None -> ""
	else
		""

let s_field cf =
	Printf.sprintf "%s|%s|%s|%s|%s|%s"
		(s_field_kind cf.cf_kind)
		(s_type_params cf.cf_params)
		(s_sig cf.cf_type)
		(s_field_flags cf)
		(s_meta cf.cf_meta)
		(s_field_body cf)

(* ---------------------------------------------------------------------- *)
(* Structural signatures (the non-field part of each decl)                 *)

let observable_class_flags = [
	CExtern; CFinal; CInterface; CAbstract; CFunctionalInterface;
]

let s_class_flags c =
	let l = List.filter (fun flag -> has_class_flag c flag) observable_class_flags in
	String.concat "," (List.map (fun f -> List.nth flag_tclass_names (int_of_class_flag f)) l)

let s_class_kind = function
	| KNormal -> "normal"
	| KTypeParameter _ -> "typeparam"
	| KExpr _ -> "expr"
	| KGeneric -> "generic"
	| KGenericInstance(c,tl) -> Printf.sprintf "geninst(%s%s)" (s_type_path c.cl_path) (s_params tl)
	| KMacroType -> "macrotype"
	| KGenericBuild _ -> "genbuild"
	| KAbstractImpl a -> Printf.sprintf "absimpl(%s)" (s_type_path a.a_path)
	| KModuleFields _ -> "modulefields"

let s_class_struct c =
	let super = match c.cl_super with
		| None -> ""
		| Some(c,tl) -> Printf.sprintf "%s%s" (s_type_path c.cl_path) (s_params tl)
	in
	let impl = String.concat "," (List.map (fun (c,tl) -> Printf.sprintf "%s%s" (s_type_path c.cl_path) (s_params tl)) c.cl_implements) in
	String.concat "\x1f" [
		"class";
		s_class_kind c.cl_kind;
		(if c.cl_private then "private" else "");
		s_class_flags c;
		s_type_params c.cl_params;
		"super=" ^ super;
		"impl=" ^ impl;
		"meta=" ^ s_meta c.cl_meta;
	]

let s_enum_struct en =
	let flags = List.filter (fun f -> has_enum_flag en f) [EnExtern] in
	String.concat "\x1f" [
		"enum";
		(if en.e_private then "private" else "");
		String.concat "," (List.map (fun f -> List.nth ["EnExtern";"EnExcluded"] (int_of_enum_flag f)) flags);
		s_type_params en.e_params;
		"names=" ^ String.concat "," en.e_names;
		"meta=" ^ s_meta en.e_meta;
	]

let s_typedef_struct td =
	String.concat "\x1f" [
		"typedef";
		(if td.t_private then "private" else "");
		s_type_params td.t_params;
		"type=" ^ s_sig td.t_type;
		"meta=" ^ s_meta td.t_meta;
	]

let s_abstract_struct a =
	let s_field_casts l = String.concat "," (List.map (fun (t,cf) -> Printf.sprintf "%s>%s" (s_sig t) cf.cf_name) l) in
	let s_op (op,cf) = Printf.sprintf "%s>%s" (s_binop op) cf.cf_name in
	let s_unop (op,flag,cf) = Printf.sprintf "%s%s>%s" (s_unop op) (match flag with Postfix -> "post" | Prefix -> "pre") cf.cf_name in
	String.concat "\x1f" [
		"abstract";
		(if a.a_private then "private" else "");
		(if a.a_extern then "extern" else "");
		(if a.a_enum then "enum" else "");
		s_type_params a.a_params;
		"this=" ^ s_sig a.a_this;
		"impl=" ^ (match a.a_impl with None -> "" | Some c -> s_type_path c.cl_path);
		"from=" ^ String.concat "," (List.map s_sig a.a_from);
		"fromField=" ^ s_field_casts a.a_from_field;
		"to=" ^ String.concat "," (List.map s_sig a.a_to);
		"toField=" ^ s_field_casts a.a_to_field;
		"array=" ^ String.concat "," (List.map (fun cf -> cf.cf_name) a.a_array);
		"read=" ^ (match a.a_read with None -> "" | Some cf -> cf.cf_name);
		"write=" ^ (match a.a_write with None -> "" | Some cf -> cf.cf_name);
		"call=" ^ (match a.a_call with None -> "" | Some cf -> cf.cf_name);
		"ops=" ^ String.concat "," (List.map s_op a.a_ops);
		"unops=" ^ String.concat "," (List.map s_unop a.a_unops);
		"meta=" ^ s_meta a.a_meta;
	]

(* ---------------------------------------------------------------------- *)
(* Header construction                                                     *)

let add_field prefix cf fields =
	PMap.add (prefix ^ cf.cf_name) (s_field cf) fields

let class_decl c =
	let fields = PMap.empty in
	let fields = List.fold_left (fun acc cf -> add_field "i:" cf acc) fields c.cl_ordered_fields in
	let fields = List.fold_left (fun acc cf -> add_field "s:" cf acc) fields c.cl_ordered_statics in
	let fields = match c.cl_constructor with None -> fields | Some cf -> PMap.add "c:" (s_field cf) fields in
	{ hd_struct = s_class_struct c; hd_fields = fields }

let enum_decl en =
	let fields = List.fold_left (fun acc name ->
		let ef = PMap.find name en.e_constrs in
		let sig_ = Printf.sprintf "%d|%s|%s|%s" ef.ef_index (s_type_params ef.ef_params) (s_sig ef.ef_type) (s_meta ef.ef_meta) in
		PMap.add ("e:" ^ name) sig_ acc
	) PMap.empty en.e_names in
	{ hd_struct = s_enum_struct en; hd_fields = fields }

let typedef_decl td =
	{ hd_struct = s_typedef_struct td; hd_fields = PMap.empty }

let abstract_decl a =
	(* The abstract's impl class is a separate TClassDecl (KAbstractImpl) whose fields are
	   captured there; here we only record the abstract's own structural signature. *)
	{ hd_struct = s_abstract_struct a; hd_fields = PMap.empty }

let decl_of_module_type mt =
	let name = snd (t_path mt) in
	let decl = match mt with
		| TClassDecl c -> class_decl c
		| TEnumDecl en -> enum_decl en
		| TTypeDecl td -> typedef_decl td
		| TAbstractDecl a -> abstract_decl a
	in
	(name,decl)

(* Build a module's header from its fully-typed [module_def]. Strips all bodies. *)
let module_header_of m =
	let decls = List.fold_left (fun acc mt ->
		let (name,decl) = decl_of_module_type mt in
		PMap.add name decl acc
	) PMap.empty m.m_types in
	{ mh_path = m.m_path; mh_decls = decls }

(* ---------------------------------------------------------------------- *)
(* Diffing                                                                 *)

type header_change =
	| HCTypeAdded of string
	| HCTypeRemoved of string
	| HCStructural of string             (* type name *)
	| HCFieldChanged of string * string  (* type name, field key *)
	| HCFieldAdded of string * string
	| HCFieldRemoved of string * string

let s_header_change = function
	| HCTypeAdded s -> Printf.sprintf "+type %s" s
	| HCTypeRemoved s -> Printf.sprintf "-type %s" s
	| HCStructural s -> Printf.sprintf "~struct %s" s
	| HCFieldChanged(t,f) -> Printf.sprintf "~field %s.%s" t f
	| HCFieldAdded(t,f) -> Printf.sprintf "+field %s.%s" t f
	| HCFieldRemoved(t,f) -> Printf.sprintf "-field %s.%s" t f

let diff_decl name old_decl new_decl acc =
	let acc = if old_decl.hd_struct <> new_decl.hd_struct then HCStructural name :: acc else acc in
	(* fields present in new: added or changed *)
	let acc = PMap.foldi (fun key new_sig acc ->
		match (try Some (PMap.find key old_decl.hd_fields) with Not_found -> None) with
		| None -> HCFieldAdded(name,key) :: acc
		| Some old_sig -> if old_sig <> new_sig then HCFieldChanged(name,key) :: acc else acc
	) new_decl.hd_fields acc in
	(* fields present only in old: removed *)
	PMap.foldi (fun key _ acc ->
		if PMap.mem key new_decl.hd_fields then acc else HCFieldRemoved(name,key) :: acc
	) old_decl.hd_fields acc

(* All the ways [new_header] differs from [old_header], field-granular plus structural flags. *)
let header_diff old_header new_header =
	let acc = PMap.foldi (fun name new_decl acc ->
		match (try Some (PMap.find name old_header.mh_decls) with Not_found -> None) with
		| None -> HCTypeAdded name :: acc
		| Some old_decl -> diff_decl name old_decl new_decl acc
	) new_header.mh_decls [] in
	PMap.foldi (fun name _ acc ->
		if PMap.mem name new_header.mh_decls then acc else HCTypeRemoved name :: acc
	) old_header.mh_decls acc

let headers_equal old_header new_header =
	header_diff old_header new_header = []

(* ---------------------------------------------------------------------- *)
(* Field-granular spare decision                                           *)

(* The field key a [dep_field] target maps to in a [header_decl]. [CfrInit] (cl_init) is not
   part of the header, so it has no key (the caller treats [None] as "any change to the type"). *)
let field_key_of_dep_field df =
	match df.dfd_kind with
	| CfrStatic -> Some ("s:" ^ df.dfd_field)
	| CfrMember -> Some ("i:" ^ df.dfd_field)
	| CfrConstructor -> Some "c:"
	| CfrInit -> None

(* Does a single dependency edge observe [changes] (the [header_diff] of the dependency)?

   - [dep_tgt = Some df]: the dependent uses a specific field. It is affected only if that field's
     own signature changed, the field was removed, or the declaring type's structure changed (the
     latter can shift what the field means). Changes to *other* fields of the dependency are
     invisible — this is the precision win.
   - [dep_tgt = None]: a module-/type-level dependency (import, inheritance, structural signature
     reference, macro). We do not know which field, so we conservatively observe structural changes
     and type additions/removals, but NOT individual field-signature changes (those are captured by
     the field-granular edges above; relying on that keeps imports from invalidating on every edit).
   - Macro-origin edges are implementation dependencies (the dependent ran a macro from the target),
     so any change to the target is observable. *)
(* [field_is_impl tn key] tells whether the field identified by header key [key] of type [tn] is an
   implementation field (inline/macro/@:generic) in the *new* header — supplied by the caller, which
   has the live module. Such a field's body is inlined/specialized into callers, so a module-level
   edge must observe its change even though it names no specific field. *)
let edge_observes_changes ~field_is_impl changes edge =
	match edge.dep_tgt_origin with
	| MDepFromMacro | MDepFromMacroDefine ->
		(* Implementation dependency: a macro can observe anything about the target (bodies, AST,
		   even unrelated state), none of which the header captures. If the target is dirty at all,
		   the dependent must be invalidated. *)
		true
	| _ ->
	match edge.dep_tgt with
	| None ->
		(* Module-/type-level edge (import, inheritance, structural reference). It does not name a
		   field, so it cannot observe ordinary field-signature changes (those are captured by the
		   field-granular edges) — but it MUST observe structure, type add/remove, field removals,
		   and changes to implementation fields whose body the dependent may have baked in. *)
		List.exists (function
			| HCStructural _ | HCTypeAdded _ | HCTypeRemoved _ -> true
			| HCFieldRemoved _ -> true
			| HCFieldChanged(n,k) | HCFieldAdded(n,k) -> field_is_impl n k
		) changes
	| Some df ->
		let tn = snd df.dfd_path in
		let key = field_key_of_dep_field df in
		List.exists (function
			| HCStructural n | HCTypeRemoved n -> n = tn
			| HCTypeAdded _ -> false
			| HCFieldChanged(n,k) | HCFieldAdded(n,k) | HCFieldRemoved(n,k) ->
				n = tn && (match key with Some key -> key = k | None -> true)
		) changes

(* Given the dependency's old and new header and the set of edges from the dependent that point at
   that dependency, decide whether the change is observable to the dependent (⇒ it must be
   invalidated). [field_is_impl] lets module-level edges detect changes to inline/macro/@:generic
   fields (whose bodies are now part of the header). *)
let dep_change_observable ~field_is_impl old_header new_header edges =
	let changes = header_diff old_header new_header in
	changes <> [] && List.exists (edge_observes_changes ~field_is_impl changes) edges

(* ---------------------------------------------------------------------- *)
(* Serialization                                                           *)

(* Self-contained binary encoding (length-prefixed strings, little-endian u32 counts/lengths).
   The header owns its own format rather than going through the hxb string pool: the leaves are
   mostly-unique signature strings that would only bloat the shared pool. [mh_path] is not stored
   (the reader already knows the module path) so [decode] takes it explicitly. *)
let encode h =
	let b = Buffer.create 256 in
	let add_int n =
		Buffer.add_char b (Char.chr (n land 0xff));
		Buffer.add_char b (Char.chr ((n asr 8) land 0xff));
		Buffer.add_char b (Char.chr ((n asr 16) land 0xff));
		Buffer.add_char b (Char.chr ((n asr 24) land 0xff))
	in
	let add_str s = add_int (String.length s); Buffer.add_string b s in
	let sorted pm = List.sort (fun (a,_) (b,_) -> compare a b) (PMap.foldi (fun k v acc -> (k,v) :: acc) pm []) in
	let decls = sorted h.mh_decls in
	add_int (List.length decls);
	List.iter (fun (name,decl) ->
		add_str name;
		add_str decl.hd_struct;
		let fields = sorted decl.hd_fields in
		add_int (List.length fields);
		List.iter (fun (k,sg) -> add_str k; add_str sg) fields
	) decls;
	Buffer.contents b

let decode mh_path data =
	let pos = ref 0 in
	let get_int () =
		let n =
			(Char.code data.[!pos]) lor (Char.code data.[!pos+1] lsl 8)
			lor (Char.code data.[!pos+2] lsl 16) lor (Char.code data.[!pos+3] lsl 24)
		in
		pos := !pos + 4;
		n
	in
	let get_str () = let n = get_int () in let s = String.sub data !pos n in pos := !pos + n; s in
	let ndecls = get_int () in
	let mh_decls = ref PMap.empty in
	for _ = 1 to ndecls do
		let name = get_str () in
		let hd_struct = get_str () in
		let nf = get_int () in
		let hd_fields = ref PMap.empty in
		for _ = 1 to nf do
			let k = get_str () in
			let sg = get_str () in
			hd_fields := PMap.add k sg !hd_fields
		done;
		mh_decls := PMap.add name { hd_struct; hd_fields = !hd_fields } !mh_decls
	done;
	{ mh_path; mh_decls = !mh_decls }
