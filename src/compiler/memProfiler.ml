(* Memprof-based promotion profiler (diagnostic, Task: "what promotes?").

   Statistically samples allocations and records the allocation backtrace of every
   sampled block that is PROMOTED minor->major (i.e. survives a minor GC). Those are
   exactly the allocations that grow the major heap / feed the peak — the ~entirety
   of the recompile's RAM cost, vs the ~97% of churn that dies young.

   Enabled by HAXE_MEMPROF=1 (rate via HAXE_MEMPROF_RATE, default 1e-4). The report is
   emitted next to the --times table (see CompilerOutput.send_timer_report) and reset
   each time, so with --times on every compile each report attributes that compile. *)

let enabled = (try Sys.getenv "HAXE_MEMPROF" = "1" with Not_found -> false)
let sampling_rate = (try float_of_string (Sys.getenv "HAXE_MEMPROF_RATE") with _ -> 1e-4)

(* allocation-site (top frames) -> number of promoted samples *)
let promoted : (string,int) Hashtbl.t = Hashtbl.create 0
(* same samples bucketed by source module (complete coverage, not just top sites) *)
let promoted_by_module : (string,int) Hashtbl.t = Hashtbl.create 0
(* ALL sampled allocations (not just promoted), by site and by module. This captures
   churn that dies young too — e.g. the macro phase allocates ~12.5GB but promotes
   ~0.2GB, so promotion tables alone can't say what that 12.5GB is. *)
let allocated : (string,int) Hashtbl.t = Hashtbl.create 0
let allocated_by_module : (string,int) Hashtbl.t = Hashtbl.create 0

(* Top frames of a backtrace, as a one-line key. Drops the leading runtime frames
   and keeps the first few user frames so identical sites group together. *)
let bt_lines (bt : Printexc.raw_backtrace) =
	let s = Printexc.raw_backtrace_to_string bt in
	List.filter (fun l -> String.trim l <> "") (String.split_on_char '\n' s)

let bt_key lines =
	let rec take n = function x :: xs when n > 0 -> x :: take (n - 1) xs | _ -> [] in
	String.concat " <- " (List.map String.trim (take 4 lines))

(* Complete-coverage bucket: the first compiler (src/) frame's file basename, so all
   promotion is attributed to a source module (not just the top-30 hot sites). Falls
   back to the innermost frame when no src/ frame is present (pure stdlib). *)
let bt_module lines =
	let basename_of l =
		try
			let i = Str.search_forward (Str.regexp {|file "\([^"]*\)"|}) l 0 in
			ignore i;
			Filename.basename (Str.matched_group 1 l)
		with Not_found -> "?"
	in
	let rec first_src = function
		| l :: rest ->
			(try
				let _ = Str.search_forward (Str.regexp {|file "src/|}) l 0 in
				basename_of l
			with Not_found -> first_src rest)
		| [] -> (match lines with l :: _ -> basename_of l | [] -> "?")
	in
	first_src lines

let bump h k = Hashtbl.replace h k (1 + (try Hashtbl.find h k with Not_found -> 0))

let on_alloc a =
	let bt = a.Gc.Memprof.callstack in
	let lines = bt_lines bt in
	bump allocated (bt_key lines);
	bump allocated_by_module (bt_module lines);
	Some bt

let tracker : (Printexc.raw_backtrace, Printexc.raw_backtrace) Gc.Memprof.tracker = {
	Gc.Memprof.alloc_minor = on_alloc;
	alloc_major = on_alloc;
	promote = (fun bt ->
		let lines = bt_lines bt in
		bump promoted (bt_key lines);
		bump promoted_by_module (bt_module lines);
		Some bt
	);
	dealloc_minor = (fun _ -> ());
	dealloc_major = (fun _ -> ());
}

let () =
	if enabled then ignore (Gc.Memprof.start ~sampling_rate ~callstack_size:12 tracker)

(* Each sample represents ~ word_size_bytes / rate of allocation (size-biased Poisson
   sampling => per-sample weight is independent of block size). *)
let report_and_reset print =
	if enabled then begin
		let bytes_per_sample = (float_of_int (Sys.word_size / 8)) /. sampling_rate in
		let mb n = float_of_int n *. bytes_per_sample /. 1048576. in
		let sorted h = List.sort (fun (_,a) (_,b) -> compare b a) (Hashtbl.fold (fun k n acc -> (k,n) :: acc) h []) in
		(* Top-30 hot sites, with the estimated total in the header. *)
		let print_sites header h =
			let l = sorted h in
			let total = List.fold_left (fun acc (_,n) -> acc + n) 0 l in
			print "";
			print (Printf.sprintf "%s (Memprof rate=%g, est ~%.0f MB total):" header sampling_rate (mb total));
			List.iteri (fun i (k,n) -> if i < 30 then print (Printf.sprintf "  ~%7.1f MB | %s" (mb n) k)) l
		in
		(* Complete per-module coverage (sums to the total above). *)
		let print_modules header h =
			print "";
			print header;
			List.iter (fun (k,n) -> if mb n >= 1.0 then print (Printf.sprintf "  ~%7.1f MB | %s" (mb n) k)) (sorted h)
		in
		print_sites "PROMOTED-TO-MAJOR BY ALLOCATION SITE" promoted;
		print_modules "PROMOTED-TO-MAJOR BY SOURCE MODULE (complete):" promoted_by_module;
		print_sites "ALLOCATED (all, incl. short-lived) BY ALLOCATION SITE" allocated;
		print_modules "ALLOCATED (all) BY SOURCE MODULE (complete):" allocated_by_module;
		Hashtbl.clear promoted;
		Hashtbl.clear promoted_by_module;
		Hashtbl.clear allocated;
		Hashtbl.clear allocated_by_module
	end
