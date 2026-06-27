type timer = {
	id : string list;
	mutable total : float;
	mutable pauses : float;
	mutable calls : int;
	(* Bytes allocated while this timer was current, exclusive of nested timers
	   (mirrors the [pauses] accounting for [total]). Only meaningful under --times. *)
	mutable alloc : float;
	mutable alloc_pauses : float;
	(* Words promoted minor->major while this timer was current (exclusive). The
	   fraction of [alloc] that survived a minor GC = what actually grows the major
	   heap / peak; the rest died young in the minor heap (cheap). *)
	mutable promoted : float;
	mutable promoted_pauses : float;
}

type measure_times =
	| Yes
	| No
	| Maybe

type timer_context = {
	root_timer : timer;
	mutable current : timer;
	mutable measure_times : measure_times;
	start_time : float;
	timer_lut : (string list,timer) Hashtbl.t;
	domain_id : Domain.id;
}

let make id = {
	id = id;
	total = 0.;
	pauses = 0.;
	calls = 0;
	alloc = 0.;
	alloc_pauses = 0.;
	promoted = 0.;
	promoted_pauses = 0.;
}

let make_context root_timer =
	let ctx = {
		root_timer = root_timer;
		current = root_timer;
		timer_lut = Hashtbl.create 0;
		measure_times = Maybe;
		start_time = Extc.time();
		domain_id = Domain.self ();
	} in
	Hashtbl.add ctx.timer_lut root_timer.id root_timer;
	ctx

let update_timer timer start =
	let now = Extc.time () in
	let dt = now -. start in
	timer.total <- timer.total +. dt -. timer.pauses;
	dt

let start_timer ctx id =
	let start = Extc.time () in
	let start_alloc = Gc.allocated_bytes () in
	let (_,start_promoted,_) = Gc.counters () in
	let old = ctx.current in
	let timer = try
		Hashtbl.find ctx.timer_lut id
	with Not_found ->
		let timer = make id in
		Hashtbl.add ctx.timer_lut id timer;
		timer
	in
	timer.calls <- timer.calls + 1;
	ctx.current <- timer;
	(fun () ->
		let dt = update_timer timer start in
		let da = Gc.allocated_bytes () -. start_alloc in
		let (_,now_promoted,_) = Gc.counters () in
		let dp = now_promoted -. start_promoted in
		timer.alloc <- timer.alloc +. da -. timer.alloc_pauses;
		timer.promoted <- timer.promoted +. dp -. timer.promoted_pauses;
		timer.alloc_pauses <- 0.;
		timer.promoted_pauses <- 0.;
		timer.pauses <- 0.;
		old.pauses <- old.pauses +. dt;
		old.alloc_pauses <- old.alloc_pauses +. da;
		old.promoted_pauses <- old.promoted_pauses +. dp;
		ctx.current <- old
	)

let start_timer ctx id = match id,ctx.measure_times with
	| (_ :: _),(Yes | Maybe) when Domain.self () = ctx.domain_id ->
		start_timer ctx id
	| _ ->
		(fun () -> ())

let time ctx id f arg =
	let close = start_timer ctx id in
	Std.finally close f arg

let determine_id level base_labels label1 label2 =
	match level,label2 with
	| 0,_ -> base_labels
	| 1,_ -> base_labels @ label1
	| _,Some label2 -> base_labels @ label1 @ [label2]
	| _ -> base_labels

let level_from_define defines define =
	try
		int_of_string (Define.defined_value defines define)
	with _ ->
		0

(* reporting *)

let timer_threshold = 0.01

type timer_node = {
	name : string;
	path : string;
	parent : timer_node;
	info : string;
	mutable time : float;
	mutable alloc : float;
	mutable promoted : float;
	mutable num_calls : int;
	mutable children : timer_node list;
}

let build_times_tree ctx =
	ignore(update_timer ctx.root_timer ctx.start_time);
	let nodes = Hashtbl.create 0 in
	let rec root = {
		name = "";
		path = "";
		parent = root;
		info = "";
		time = 0.;
		alloc = 0.;
		promoted = 0.;
		num_calls = 0;
		children = [];
	} in
	Hashtbl.iter (fun _ timer ->
		let rec loop parent sl = match sl with
			| [] -> Globals.die "" __LOC__
			| s :: sl ->
				let path = (match parent.path with "" -> "" | _ -> parent.path ^ ".") ^ s in
				let node = try
					let node = Hashtbl.find nodes path in
					node.num_calls <- node.num_calls + timer.calls;
					node.time <- node.time +. timer.total;
					node.alloc <- node.alloc +. timer.alloc;
					node.promoted <- node.promoted +. timer.promoted;
					node
				with Not_found ->
					let name,info = try
						let i = String.rindex s '.' in
						String.sub s (i + 1) (String.length s - i - 1),String.sub s 0 i
					with Not_found ->
						s,""
					in
					let node = {
						name = name;
						path = path;
						parent = parent;
						info = info;
						time = timer.total;
						alloc = timer.alloc;
						promoted = timer.promoted;
						num_calls = timer.calls;
						children = [];
					} in
					Hashtbl.add nodes path node;
					node
				in
				begin match sl with
					| [] -> ()
					| _ ->
						let child = loop node sl in
						if not (List.memq child node.children) then
							node.children <- child :: node.children;
				end;
				node
		in
		let node = loop root timer.id in
		if not (List.memq node root.children) then
			root.children <- node :: root.children
	) ctx.timer_lut;
	let max_name = ref 0 in
	let max_calls = ref 0 in
	let rec loop depth node =
		let l = (String.length node.name) + 2 * depth in
		List.iter (fun child ->
			if depth = 0 then begin
				node.num_calls <- node.num_calls + child.num_calls;
				node.time <- node.time +. child.time;
				node.alloc <- node.alloc +. child.alloc;
				node.promoted <- node.promoted +. child.promoted;
			end;
			loop (depth + 1) child;
		) node.children;
		node.children <- List.sort (fun node1 node2 -> compare node2.time node1.time) node.children;
		if node.num_calls > !max_calls then max_calls := node.num_calls;
		if node.time >= timer_threshold && l > !max_name then max_name := l;
	in
	loop 0 root;
	!max_name,!max_calls,root

let report_times ctx print =
	let max_name,max_calls,root = build_times_tree ctx in
	let max_calls = String.length (string_of_int max_calls) in
	(* promoted is in words; *8 -> bytes -> MB. promoted/alloc = the fraction of
	   churn that survived a minor GC (what grows the major heap / peak). *)
	let mb_alloc node = node.alloc /. 1048576. in
	let mb_prom node = node.promoted *. 8. /. 1048576. in
	print (Printf.sprintf "%-*s | %7s | %9s | %9s |   %% |  p%% | %*s | info" max_name "name" "time(s)" "alloc(MB)" "promo(MB)" max_calls "#");
	let sep = String.make (max_name + max_calls + 51) '-' in
	print sep;
	let print_time name node =
		if node.time >= timer_threshold then
			print (Printf.sprintf "%-*s | %7.3f | %9.1f | %9.1f | %3.0f | %3.0f | %*i | %s" max_name name node.time (mb_alloc node) (mb_prom node) (node.time *. 100. /. root.time) (node.time *. 100. /. node.parent.time) max_calls node.num_calls node.info)
	in
	let rec loop depth node =
		let name = (String.make (depth * 2) ' ') ^ node.name in
		print_time name node;
		List.iter (loop (depth + 1)) node.children
	in
	List.iter (loop 0) root.children;
	print sep;
	print_time "total" root;
	(* Flat top-allocators view (Task: per-macro/per-module churn attribution). The
	   tree hides sub-threshold leaves; this lists every timer by alloc regardless of
	   time, so individual macros/modules responsible for churn are visible. *)
	let all = ref [] in
	let rec collect node = all := node :: !all; List.iter collect node.children in
	List.iter collect root.children;
	let all = List.sort (fun a b -> compare b.alloc a.alloc) !all in
	let rec take n = function x :: xs when n > 0 -> x :: take (n - 1) xs | _ -> [] in
	print "";
	print (Printf.sprintf "TOP ALLOCATORS (flat, by alloc) | %9s | %9s | %s" "alloc(MB)" "promo(MB)" "calls | path");
	print sep;
	List.iter (fun node ->
		if node.alloc /. 1048576. >= 1.0 then
			print (Printf.sprintf "%-31s | %9.1f | %9.1f | %5i | %s" "" (mb_alloc node) (mb_prom node) node.num_calls node.path)
	) (take 40 all)