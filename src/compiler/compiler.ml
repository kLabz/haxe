open Globals
open Message
open Common
open ParsedArg

let error_ext com (err : Error.error) =
	com.error_ext err

let error com msg p =
	error_ext com (Error.make_error (Custom msg) p)

let has_error com =
	com.part_scope.has_error

let handle_diagnostics com ?(diagnostics_kind = MessageKind.DKCompilerMessage) msg p message_kind =
	com.part_scope.has_error <- true;
	add_diagnostics_message ~diagnostics_kind com msg p message_kind;
	match com.part_scope.report_mode with
	| RMDiagnostics _ -> DisplayOutput.emit_diagnostics com
	| _ -> die "" __LOC__

let run_or_diagnose com f =
	if is_diagnostics com then begin try
			f ()
		with
		| Error.Error err ->
			com.part_scope.has_error <- true;
			Error.recurse_error (fun depth err ->
				add_diagnostics_message ~depth com (Error.error_msg err.err_message) err.err_pos MKError
			) err;
			(match com.part_scope.report_mode with
			| RMDiagnostics _ -> DisplayOutput.emit_diagnostics com
			| _ -> die "" __LOC__)
		| Parser.Error(msg,p) ->
			handle_diagnostics com ~diagnostics_kind:DKParserError (Parser.error_msg msg) p MKError
		| Lexer.Error(msg,p) ->
			handle_diagnostics com ~diagnostics_kind:DKParserError (Lexer.error_msg msg) p MKError
		end
	else
		f ()

let run_command com cmd =
	let io = com.request_scope.io in
	(* TODO: this is a hack *)
	let cmd = if com.sctx.is_server then begin
		let h = Hashtbl.create 0 in
		Hashtbl.add h "__file__" com.file;
		Hashtbl.add h "__platform__" (platform_name com.platform);
		Helper.expand_env ~h:(Some h) cmd
	end else
		cmd
	in
	let len = String.length cmd in
	let result =
		if len > 3 && String.sub cmd 0 3 = "cd " then begin
			Sys.chdir (String.sub cmd 3 (len - 3));
			0
		end else if not com.sctx.is_server then
			(* In non-server mode, inherit stdin/stdout/stderr so that interactive commands work *)
			Sys.command cmd
		else begin
			(* In server mode, capture stdout/stderr through the output target and
			   forward the client's stdin from request_scope. *)
			PipeThings.run_command io cmd
		end
	in
	result

let run_command com cmd =
	Timer.time com.timer_ctx ["command";cmd] (run_command com) cmd

module Setup = struct
	let initialize_target com actx =
		init_platform com;
		com.class_paths#lock_context com.custom_ext (platform_name com.platform) false;
		let add_std dir =
			com.class_paths#modify_inplace (fun cp -> match cp#scope with
				| Std ->
					let cp' = new ClassPath.directory_class_path (cp#path ^ dir ^ "/_std/") StdTarget in
					cp :: [cp']
				| _ ->
					[cp]
			);
		in
		match com.platform with
			| Cross ->
				"?"
			| CustomTarget name ->
				name
			| Flash ->
				let rec loop = function
					| [] -> ()
					| (v,_) :: _ when v > com.flash_version -> ()
					| (v,def) :: l ->
						Common.raw_define com ("flash" ^ def);
						loop l
				in
				loop Common.flash_versions;
				com.package_rules <- PMap.remove "flash" com.package_rules;
				add_std "flash";
				"swf"
			| Neko ->
				add_std "neko";
				"n"
			| Js ->
				let es_version =
					try
						int_of_string (Common.defined_value com Define.JsEs)
					with
					| Not_found ->
						(Common.define_value com Define.JsEs "5"; 5)
					| _ ->
						0
				in

				if es_version < 5 then
					failwith "Invalid -D js-es value, minimal supported version is 5";

				if es_version >= 5 then Common.raw_define com "js_es5"; (* backward-compatibility *)

				add_std "js";
				"js"
			| Lua ->
				add_std "lua";
				"lua"
			| Php ->
				add_std "php";
				"php"
			| Cpp ->
				Common.define_value com Define.HxcppApiLevel "500";
				add_std "cpp";
				if Common.defined com Define.Cppia then
					actx.classes <- (Path.parse_path "cpp.cppia.HostClasses" ) :: actx.classes;
				"cpp"
			| Jvm ->
				add_std "jvm";
				com.package_rules <- PMap.remove "java" com.package_rules;
				"java"
			| Python ->
				add_std "python";
				if not (Common.defined com Define.PythonVersion) then
					Common.define_value com Define.PythonVersion "3.3";
				"python"
			| Hl ->
				add_std "hl";
				if not (Common.defined com Define.HlVer) then begin
					let hl_ver = try
						Std.input_file (Common.find_file com "hl/hl_version")
					with Not_found ->
						failwith "The file hl_version could not be found. Please make sure HAXE_STD_PATH is set to the standard library corresponding to the used compiler version."
					in
					Define.define_value com.defines Define.HlVer hl_ver
				end;
				"hl"
			| Eval ->
				add_std "eval";
				"eval"

	let init_native_libs com native_libs =
		(* Native lib pass 1: Register *)
		let fl = List.map (fun lib -> NativeLibraryHandler.add_native_lib com lib) (List.rev native_libs) in
		(* Native lib pass 2: Initialize *)
		List.iter (fun f -> f()) fl

	let create_typer_context com macros =
		let buffer = Buffer.create 64 in
		Buffer.add_string buffer "Defines: ";
		PMap.iter (fun k v -> match v with
			| "1" -> Printf.bprintf buffer "%s;" k
			| _ -> Printf.bprintf buffer "%s=%s;" k v
		) com.defines.values;
		Buffer.truncate buffer (Buffer.length buffer - 1);
		Common.log com (Buffer.contents buffer);
		com.callbacks#run com.error_ext com.callbacks#get_before_typer_create;
		TyperEntry.create com macros

	let executable_path() =
		Extc.executable_path()

	open ClassPath

	let get_std_class_paths () =
		try
			let p = Sys.getenv "HAXE_STD_PATH" in
			let p = Path.remove_trailing_slash p in
			let rec loop = function
				| drive :: path :: l ->
					if String.length drive = 1 && ((drive.[0] >= 'a' && drive.[0] <= 'z') || (drive.[0] >= 'A' && drive.[0] <= 'Z')) then
						(drive ^ ":" ^ path) :: loop l
					else
						drive :: loop (path :: l)
				| l ->
					l
			in
			let parts = Str.split_delim (Str.regexp "[;:]") p in
			List.map (fun s -> s,Std) (loop parts)
		with Not_found ->
			let base_path = Path.get_real_path (try executable_path() with _ -> "./") in
			if Sys.os_type = "Unix" then
				let prefix_path = Filename.dirname base_path in
				let lib_path = Filename.concat prefix_path "lib" in
				let share_path = Filename.concat prefix_path "share" in
				[
					(Filename.concat share_path "haxe/std"),Std;
					(Filename.concat lib_path "haxe/std"),Std;
					(Filename.concat base_path "std"),Std;
				]
			else
				[
					(Filename.concat base_path "std"),Std;
				]

	let init_std_class_paths com =
		List.iter (fun (s,scope) ->
			try if Sys.is_directory s then
				let cp = new ClassPath.directory_class_path (Path.add_trailing_slash s) scope in
				com.class_paths#add cp
			with Sys_error _ -> ()
		) (List.rev (get_std_class_paths ()));
		com.class_paths#add com.empty_class_path

	let setup_common_context com =
		Common.define_value com Define.HaxeVer (Printf.sprintf "%.3f" (float_of_int version /. 1000.));
		Common.define_value com Define.Haxe s_version;
		Common.raw_define com "true";
		List.iter (fun (k,v) -> Define.raw_define_value com.defines k v) DefineList.default_values;
		com.info <- CompilerMessage.default_info_handler com;
		com.warning <- CompilerMessage.default_warning_handler com;
		com.error_ext <- CompilerMessage.default_error_handler com;
		com.error <- (fun msg p -> com.error_ext (Error.make_error (Custom msg) p));
		let filter_messages = (fun keep_errors predicate -> (List.filter (fun cm ->
			(match cm_severity cm with
			| MessageSeverity.Error -> keep_errors;
			| Information | Warning | Hint -> predicate cm;)
		) (List.rev com.part_scope.messages))) in
		com.get_messages <- (fun () -> (List.map (fun cm ->
			(match cm_severity cm with
			| MessageSeverity.Error -> die "" __LOC__;
			| Information | Warning | Hint -> cm;)
		) (filter_messages false (fun _ -> true))));
		com.filter_messages <- (fun predicate -> (com.part_scope.messages <- (List.rev (filter_messages true predicate))));
		com.run_command <- run_command com;
		init_std_class_paths com

end

let check_defines com =
	if defined com Define.DisableParallelism then Parallel.enable := false;
	PMap.iter (fun k v ->
		try
			let reason = Hashtbl.find Define.deprecation_lut k in
			let p = fake_pos ("-D " ^ k) in
			begin match reason with
			| DueTo reason ->
				com.warning WDeprecatedDefine [] reason p
			| InFavorOf d ->
				Define.raw_define_value com.defines d v;
				com.warning WDeprecatedDefine [] (Printf.sprintf "-D %s has been deprecated in favor of -D %s" k d) p
			end;
		with Not_found ->
			()
	) com.defines.values

(* Phase 1 header invalidation (opt-in via [hxb.header-invalidation]): re-type the dirty frontier
   (changed source modules) before the main typing pass and drain the typing passes so their
   inferred / macro-generated signatures are resolved, then snapshot fresh headers into
   [module_lut]. [ServerCache.dependency_change_observable] compares these against the cached (old)
   headers to spare dependents whose used signatures did not change. Compile errors are ignored
   here: a genuine error in a reachable module resurfaces in the main pass below. *)
let retype_dirty_frontier com tctx =
	let dbg = Define.raw_defined com.defines "hxb.header_stats" in
	match ServerCache.collect_dirty_frontier com with
	| [] ->
		if dbg then Printf.eprintf "[header-invalidation] frontier: 0 modules\n%!"
	| paths ->
		if dbg then Printf.eprintf "[header-invalidation] frontier: %d modules [%s]\n%!"
			(List.length paths) (String.concat ", " (List.map s_type_path paths));
		(* Snapshot what's already in the context (init-macro / display modules) so we only record
		   headers for what the pre-phase itself pulls in below. *)
		let before = Hashtbl.create 0 in
		com.module_lut#iter (fun path _ -> Hashtbl.replace before path ());
		(* Stage-0 measurement: hxb full vs partial restores during the pre-phase (cumulative counters,
		   so snapshot the delta) and the pre-phase wall time -- the number that decides Phase 2's worth. *)
		let full0 = !(com.hxb_reader_stats.modules_fully_restored) in
		let part0 = !(com.hxb_reader_stats.modules_partially_restored) in
		let t0 = Extc.time () in
		(* Phase 2 increment 2 (WIP, gated): restore the pre-phase's CLEAN dependency peers signature-
		   only so re-typing an edited seed does not type the whole SCC's bodies. Invalidation is sound
		   under this (HeaderInvalidation passes); the remaining flag-on failures are partial restore not
		   yet being structurally complete enough for a re-typed seed's peer references (increment 3). *)
		let partial = Define.defined com.defines Define.HxbPrephasePartial in
		(* Increment 3 (WIP, gated): swap a throwaway module_lut onto com for the duration of the
		   pre-phase, seeded with what is already loaded (init-macro / display modules) so seeds can
		   still resolve them. The pre-phase's re-typed seeds and restored peers land in the throwaway
		   and are dropped on restore, so they never reach the main compile's module_lut / codegen;
		   only header_deltas (recorded below, before the restore) are handed back. *)
		let isolate = Define.defined com.defines Define.HxbPrephaseIsolate in
		let saved_lut = com.module_lut in
		let fresh_lut = if isolate then begin
			let fresh = new module_lut in
			saved_lut#iter (fun path m -> fresh#add path m);
			com.module_lut <- fresh;
			Some fresh
		end else None in
		let restore_lut () = if isolate then com.module_lut <- saved_lut in
		(* The isolated pre-phase is a throwaway computation: it re-types seeds and (under partial mode)
		   signature-only restores their clean peers into a throwaway lut, solely to record header deltas.
		   Its compiler messages are NOT user-facing -- the real compile re-types/restores everything and
		   emits every genuine warning/error. A seed that inlines a partial peer whose body was deferred
		   (cf_expr = None) produces spurious WInlineOptimizedField warnings and "Recursive inline" /
		   "Recursive array get" errors (verified: codegen output is identical with the pre-phase off vs
		   on). These must not leak out. The try/with guards below only catch errors that PROPAGATE as
		   exceptions; the typer's own recovery (raise_or_display_error -> com.error_ext) records many
		   errors WITHOUT re-raising, and warnings never raise -- both reach the user unless muted. So mute
		   the warning/error message channels for the duration of the isolated pre-phase and roll back
		   has_error (an inline failure on a discarded peer must not mark the whole compilation as failed). *)
		let saved_warning = com.warning in
		let saved_error = com.error in
		let saved_error_ext = com.error_ext in
		let saved_has_error = com.part_scope.has_error in
		if isolate then begin
			com.warning <- (fun ?depth:_ _ _ _ _ -> ());
			com.error <- (fun _ _ -> ());
			com.error_ext <- (fun _ -> ())
		end;
		let restore_messages () =
			if isolate then begin
				com.warning <- saved_warning;
				com.error <- saved_error;
				com.error_ext <- saved_error_ext;
				com.part_scope.has_error <- saved_has_error
			end
		in
		if partial then begin
			ServerCache.prephase_partial_mode := true;
			Hashtbl.clear ServerCache.prephase_partial_paths
		end;
		(* The isolated pre-phase is a throwaway: its only sound output is the header deltas recorded as it
		   goes; the re-typed modules are dropped. Re-typing seeds/peers from a partially-restored closure
		   exercises inline / default-arg / unification paths that can fail in MANY ways (Error.Error,
		   Unify_error, ...), and several of those paths have no in-typer recovery, so the exception
		   propagates. Rather than chase each raise site, contain ALL of them at the pre-phase boundary:
		   any failure just means fewer deltas recorded -> more conservative (sound) invalidation, and the
		   real compile re-types/restores everything and surfaces every genuine error properly. Only
		   Stack_overflow / Out_of_memory propagate (genuine resource exhaustion, unsafe to swallow). This
		   is sound precisely because the work is discarded; it is NOT a blanket excuse elsewhere. *)
		let protect f =
			try f ()
			with
			| Stack_overflow | Out_of_memory as e -> raise e
			| Error.Error _ | Error.Fatal_error _ -> ()
			(* The broad catch is sound ONLY for the isolated pre-phase, whose modules are dropped. The
			   non-isolated pre-phase types into the shared lut the main compile reuses, so there any other
			   exception must propagate (to the outer handler) rather than be silently contained. *)
			| _ when isolate -> ()
		in
		(try
			List.iter (fun mpath ->
				protect (fun () ->
					ignore (tctx.Typecore.g.Typecore.do_load_module tctx mpath null_pos);
					Typecore.flush_pass tctx.g PBuildClass "header-prephase")
			) paths;
			protect (fun () -> Typecore.flush_pass tctx.g PFinal "header-prephase");
			(* Re-typing the frontier drags in its whole dirty closure (cyclic peers etc.); the outer
			   cascade reaches the seeds *through* those peers, so record a fresh header for every module
			   the pre-phase pulled in, not just the seeds. All of it is post-PFinal so inline cf_expr
			   bodies are accurate. *)
			protect (fun () -> ServerCache.record_prephase_closure com before)
		with e ->
			(* Only Stack_overflow / Out_of_memory reach here; restore state before propagating. *)
			restore_lut ();
			restore_messages ();
			if partial then ServerCache.prephase_partial_mode := false;
			raise e);
		restore_lut ();
		restore_messages ();
		if partial then
			ServerCache.prephase_partial_mode := false;
		(* Hot-swap: a pre-phase leak produces distinct (stale) type objects for paths the throwaway loop
		   pulled in; those leak into the main compile and clash with the canonical objects as duplicate
		   identity, and -- worse -- sit inside canonical classes' super/implements chains. Tag every stale
		   object (in [fresh], not pre-existing in [before], same signature as this compile) with a
		   self-forward sentinel, and point the resolvers at the live com.module_lut. follow_* then resolves
		   any stale object to its canonical wherever the type graph is traversed during unification
		   (compared pair, super/implements walk, type-parameter constraints), so duplicates coincide. *)
		(match fresh_lut with
		| Some fresh ->
			let open Type in
			let main_sign = Define.get_signature com.defines in
			let canon_type path = (com.module_lut#find_by_type path).m_types in
			TFunctions.class_resolver := (fun c ->
				try (match List.find (fun mt -> t_path mt = c.cl_path) (canon_type c.cl_path) with TClassDecl k -> k | _ -> c) with Not_found -> c);
			TFunctions.enum_resolver := (fun e ->
				try (match List.find (fun mt -> t_path mt = e.e_path) (canon_type e.e_path) with TEnumDecl k -> k | _ -> e) with Not_found -> e);
			TFunctions.abstract_resolver := (fun a ->
				try (match List.find (fun mt -> t_path mt = a.a_path) (canon_type a.a_path) with TAbstractDecl k -> k | _ -> a) with Not_found -> a);
			TFunctions.typedef_resolver := (fun t ->
				try (match List.find (fun mt -> t_path mt = t.t_path) (canon_type t.t_path) with TTypeDecl k -> k | _ -> t) with Not_found -> t);
			fresh#iter (fun path m ->
				if not (Hashtbl.mem before path) && m.m_extra.m_sign = main_sign then
					List.iter (function
						| TClassDecl c -> c.cl_forward <- Some c
						| TEnumDecl e -> e.e_forward <- Some e
						| TAbstractDecl a -> a.a_forward <- Some a
						| TTypeDecl t -> t.t_forward <- Some t
					) m.m_types
			)
		| None -> ());
		(* Step 2 (shared seed materialization): the isolated computation above produced only header
		   deltas; its re-typed seeds live in the throwaway lut. A spared dependent in the main pass
		   resolves its reference to a dirty seed eagerly (the seed is still MSBad/tainted in the cache),
		   which dies with BadModule unless the seed is present in the SHARED lut. So re-type the dirty
		   SEEDS only (not their whole closure) from source into the now-restored shared lut, with FULL
		   peer restores. Cheap relative to the isolated phase: only the genuinely-edited seeds are
		   re-typed; their dependencies are restored from the cache, not re-typed. The PFinal flush is
		   load-bearing: inline / @:generic cf_expr bodies are not forced until PFinal, so without it a
		   spared dependent that inlines a seed's field finds cf_expr = None (recursive array get / no
		   inline). The committed non-isolated pre-phase relies on the same flush. *)
		if isolate then begin
			List.iter (fun mpath ->
				(* Same guard shape as the loop above: keep the PBuildClass flush inside the try so a
				   delayed default-arg typing error cannot escape (mirrors do_load_module's handling). *)
				(try
					ignore (tctx.Typecore.g.Typecore.do_load_module tctx mpath null_pos);
					Typecore.flush_pass tctx.g PBuildClass "header-prephase-seed"
				 with Error.Error _ | Error.Fatal_error _ -> ())
			) paths;
			(try
				Typecore.flush_pass tctx.g PFinal "header-prephase-seed"
			with Error.Error _ | Error.Fatal_error _ ->
				())
		end;
		if dbg then begin
			let full = !(com.hxb_reader_stats.modules_fully_restored) - full0 in
			let part = !(com.hxb_reader_stats.modules_partially_restored) - part0 in
			(* [part] counts every restore; full restores are a subset, so partial-only = part - full. *)
			Printf.eprintf "[header-invalidation] frontier closure typed: %d MCode modules | hxb restores: full=%d partial-only=%d | pre-phase wall=%.0fms\n%!"
				ServerCache.spare_stats.sp_retyped full (part - full) ((Extc.time () -. t0) *. 1000.);
			(* Same key counts on the captured server-message channel (print_endline -> stdout) so a
			   server test can assert the pre-phase work: [typed] = MCode modules fully re-typed in the
			   pre-phase (the perf cost we drive toward the seed count), [partial] = peers restored
			   signature-only instead. eprintf above is for interactive runs; this line is for tests. *)
			print_endline (Printf.sprintf "[header-prephase] typed=%d partial=%d"
				ServerCache.spare_stats.sp_retyped (Hashtbl.length ServerCache.prephase_partial_paths))
		end

(** Creates the typer context and types [classes] into it. *)
let do_type com mctx actx display_file_dot_path =
	let cs = com.cs in
	CommonCache.maybe_add_context_sign cs com "before_init_macros";
	enter_stage com CInitMacrosStart;
	ServerMessage.compiler_stage com;
	Setup.init_native_libs com actx.hxb_libs;
	let mctx = List.fold_left (fun mctx path ->
		Some (MacroContext.call_init_macro com mctx path)
	) mctx (List.rev actx.config_macros) in
	enter_stage com CInitMacrosDone;
	check_defines com;
	update_platform_config com; (* make sure to adapt all flags changes defined during init macros *)
	ServerMessage.compiler_stage com;

	let macros = match mctx with None -> None | Some mctx -> mctx.g.macros in
	Setup.init_native_libs com actx.native_libs;
	let tctx = Setup.create_typer_context com macros in
	(* Reset the hot-swap resolvers from any prior compile (they persist via global refs and are re-armed
	   by retype_dirty_frontier only when the isolated pre-phase runs). *)
	TFunctions.class_resolver := (fun c -> c);
	TFunctions.enum_resolver := (fun e -> e);
	TFunctions.abstract_resolver := (fun a -> a);
	TFunctions.typedef_resolver := (fun t -> t);
	(* Duplicate-identity unify counter (gated by -D hxb.header_stats): measures how many "X should be
	   X" failures the hot-swap eliminates (0 on a correct build). *)
	if Define.raw_defined com.defines "hxb.header_stats" then begin
		TUnification.dup_identity_trace := true;
		TUnification.dup_identity_count := 0;
		Hashtbl.clear TUnification.dup_identity_paths
	end;
	(* Print the duplicate-identity tally even if typing aborts (a raw Unify_error from a leaked
	   duplicate can escape do_type before CTypingDone, which is exactly the case we want to measure). *)
	let print_dup_tally where =
		if Define.raw_defined com.defines "hxb.header_stats" then
			Printf.eprintf "[dup-identity v=mark1 %s] total=%d distinct-modules=%d [%s]\n%!"
				where !TUnification.dup_identity_count (Hashtbl.length TUnification.dup_identity_paths)
				(String.concat ", " (Hashtbl.fold (fun p () acc -> s_type_path p :: acc) TUnification.dup_identity_paths []))
	in
	(* Re-type the dirty frontier and snapshot fresh headers BEFORE anything walks the module graph
	   (display-file load below, check_display_file, main typing). Those walks make the cache-reuse
	   decision via ServerCache.dependency_change_observable, which needs the header deltas already
	   populated; running this later left every lookup hitting an empty table. *)
	retype_dirty_frontier com tctx;
	let display_file_dot_path = DisplayProcessing.maybe_load_display_file_before_typing tctx display_file_dot_path in
	(* Make sure display module is being typed *)
	Option.may (fun cpath -> actx.classes <- cpath :: actx.classes) display_file_dot_path;
	DumpConfig.update_from_defines com.part_scope.dump_config com.defines;
	CommonCache.lock_signature com "after_init_macros";
	Option.may (fun mctx -> MacroContext.finalize_macro_api tctx mctx) mctx;
	(try
		(try begin
			com.callbacks#run com.error_ext com.callbacks#get_after_init_macros;
			run_or_diagnose com (fun () ->
				if com.display.dms_kind <> DMNone then DisplayTexpr.check_display_file tctx cs;
				List.iter (fun cpath ->
					ignore(tctx.Typecore.g.Typecore.do_load_module tctx cpath null_pos);
					Typecore.flush_pass tctx.g PBuildClass "actx.classes"
				) (List.rev actx.classes);
				Finalization.finalize tctx;
			);
		end with TypeloadParse.DisplayInMacroBlock ->
			ignore(DisplayProcessing.load_display_module_in_macro tctx display_file_dot_path true)
		)
	with e ->
		print_dup_tally "aborted";
		raise e);
	enter_stage com CTypingDone;
	print_dup_tally "typing-done";
	ServerMessage.compiler_stage com;
	(* If we are trying to find references, let's syntax-explore everything we know to check for the
		identifier we are interested in. We then type only those modules that contain the identifier. *)
	begin match com.display.dms_kind with
		| (DMUsage _ | DMImplementation) -> FindReferences.find_possible_references tctx cs;
		| _ -> ()
	end;
	(tctx, display_file_dot_path)

let finalize_typing com tctx =
	let main_module = Finalization.maybe_load_main tctx in
	enter_stage com CFilteringStart;
	ServerMessage.compiler_stage com;
	let (main_expr,main_file),types,modules = run_or_diagnose com (fun () -> Finalization.generate tctx main_module) in
	com.main.main_expr <- main_expr;
	com.main.main_file <- main_file;
	com.types <- types;
	com.modules <- modules

let finalize_typing com tctx =
	Timer.time com.timer_ctx ["finalize"] (finalize_typing com) tctx

let filter com tctx ectx before_destruction =
	Timer.time com.timer_ctx ["filters"] (fun () ->
		run_or_diagnose com (fun () -> Filters.run tctx ectx before_destruction)
	) ()

let compile com actx sctx =
	(* Set up display configuration *)
	DisplayProcessing.process_display_configuration com;
	let restore = disable_report_mode com in
	let display_file_dot_path = DisplayProcessing.process_display_file com actx in
	restore ();
	let mctx = match com.platform with
		| CustomTarget name ->
			begin try
				Some (MacroContext.call_init_macro com None (Printf.sprintf "%s.Init.init()" name))
			with (Error.Error { err_message = Module_not_found ([pack],"Init") }) when pack = name ->
				(* ignore if <target_name>.Init doesn't exist *)
				None
			end
		| _ ->
			None
		in
	(* Initialize target: This allows access to the appropriate std packages and sets the -D defines. *)
	let ext = Setup.initialize_target com actx in
	update_platform_config com; (* make sure to adapt all flags changes defined after platform *)
	ServerCache.after_target_init sctx com;
	Timer.time com.timer_ctx ["init"] (fun () ->
		List.iter (fun f -> f()) (List.rev (actx.pre_compilation));
		begin match actx.hxb_out with
			| None ->
				()
			| Some file ->
				com.hxb_writer_config <- HxbWriterConfig.process_argument file
		end;
	) ();
	enter_stage com CInitialized;
	ServerMessage.compiler_stage com;
	if actx.classes = [([],"Std")] && not actx.force_typing then begin
		if actx.cmds = [] && not actx.did_something then actx.raise_usage();
	end else begin
		(* Actual compilation starts here *)
		let (tctx,display_file_dot_path) = Timer.time com.timer_ctx ["typing"] (do_type com mctx actx) display_file_dot_path in
		if DisplayProcessing.handle_display_after_typing com tctx display_file_dot_path then raise CompilerMessage.Abort;
		let ectx = ExceptionInit.create_exception_context tctx in
		finalize_typing com tctx;
		Dump.maybe_generate_dump com AfterTyping;
		let is_compilation = is_compilation com in
		com.callbacks#add_after_save (fun () ->
			ServerCache.after_save sctx com;
			if is_compilation then match com.hxb_writer_config with
				| Some config ->
					Generate.check_hxb_output com config;
				| None ->
					()
		);
		if is_diagnostics com then
			filter com com ectx (fun () -> DisplayProcessing.handle_display_after_finalization com tctx display_file_dot_path)
		else begin
			DisplayProcessing.handle_display_after_finalization com tctx display_file_dot_path;
			filter com com ectx (fun () -> ());
		end;
		if has_error com && is_compilation then raise CompilerMessage.Abort;
		if is_compilation then Generate.check_auxiliary_output com actx;
		enter_stage com CGenerationStart;
		ServerMessage.compiler_stage com;
		Dump.maybe_generate_dump com AfterDce;
		Generate.maybe_generate_dump_dependencies com tctx;
		if not actx.no_output then Generate.generate com tctx ext actx;
		enter_stage com CGenerationDone;
		ServerMessage.compiler_stage com;
	end;
	Sys.catch_break false;
	com.callbacks#run com.error_ext com.callbacks#get_after_generation;
	if not actx.no_output then begin
		List.iter (fun c ->
			let r = run_command com c in
			if r <> 0 then failwith ("Command failed with error " ^ string_of_int r)
		) (List.rev actx.cmds)
	end

let make_ice_message (com : Common.context) msg backtrace =
		let ver = (s_version_full com.sctx.version) in
		let os_type = if Sys.unix then "unix" else "windows" in
		Printf.sprintf "%s\nHaxe: %s; OS type: %s;\n%s" msg ver os_type backtrace
let compile_safe com f =
try
	f ()
with
	| Error.Fatal_error err ->
		error_ext com err
	| Lexer.Error (m,p) ->
		error com (Lexer.error_msg m) p
	| Parser.Error (m,p) ->
		error com (Parser.error_msg m) p
	| Typecore.Forbid_package ((pack,m,p),pl,pf)  ->
		if com.display.dms_kind <> DMNone && com.part_scope.has_next then begin
			com.part_scope.has_error <- false;
			com.part_scope.messages <- [];
		end else begin
			let sub = List.map (fun p -> Error.make_error (Error.Custom (Error.compl_msg "referenced here")) p) pl in
			error_ext com (Error.make_error (Error.Custom (Printf.sprintf "You cannot access the %s package while %s (for %s)" pack (if pf = "macro" then "in a macro" else "targeting " ^ pf) (s_type_path m))) ~sub p)
		end
	| Error.Error err ->
		error_ext com err
	| Arg.Bad msg ->
		error com ("Error: " ^ msg) null_pos
	| Failure msg when is_diagnostics com ->
		handle_diagnostics com msg null_pos MKError;
	| Failure msg when not Helper.is_debug_run ->
		error com ("Error: " ^ msg) null_pos
	| Globals.Ice (msg,backtrace) when is_diagnostics com ->
		let s = make_ice_message com msg backtrace in
		handle_diagnostics com s null_pos MKError
	| Globals.Ice (msg,backtrace) when not Helper.is_debug_run ->
		let s = make_ice_message com msg backtrace in
		error com ("Error: " ^ s) null_pos
	| Helper.HelpMessage msg ->
		CompilerIo.write_out com.request_scope.io (msg ^ "\n")
	| Parser.TypePath (p,c,is_import,pos) ->
		DisplayOutput.handle_type_path_exception com p c is_import pos
	| Parser.SyntaxCompletion(kind,subj) ->
		DisplayOutput.handle_syntax_completion com kind subj;
		error com ("Error: No completion point was found") null_pos
	| DisplayException.DisplayException dex ->
		DisplayOutput.handle_display_exception com dex
	| CompilerMessage.Abort | Out_of_memory | EvalTypes.Sys_exit _ | Hlinterp.Sys_exit _ | DisplayJson.JsonCompleted as exc ->
		(* We don't want these to be caught by the catchall below *)
		raise exc
	| e when (try Sys.getenv "OCAMLRUNPARAM" <> "b" with _ -> true) && not Helper.is_debug_run ->
		error com (Printexc.to_string e) null_pos

let compile_safe com f =
	try compile_safe com f with CompilerMessage.Abort -> ()

let finalize com =
	CompilerIo.flush com.request_scope.io;
	List.iter (fun lib -> lib#close) com.hxb_libs;
	(* In server mode any open libs are closed by the lib_build_task. In offline mode
		we should do it here to be safe. *)
	if not com.sctx.is_server then begin
		List.iter (fun lib -> lib#close) com.native_libs.java_libs;
		List.iter (fun lib -> lib#close) com.native_libs.swf_libs;
	end

module ContextFlush = struct
	let flush_context com =
		match com.part_scope.report_mode with
		| RMDiagnostics _ ->
			(* In diagnostics mode, messages are already in the unified buffer.
			   Output happens via DisplayOutput.emit_diagnostics, not flush_messages. *)
			()
		| _ ->
			let rh = com.request_scope.result_handler in
			CompilerOutput.flush_messages rh (Common.has_error_to_report com) com
end

let catch_completion_and_exit com sctx run =
	try
		run com;
		if has_error com then 1 else 0
	with
		| DisplayJson.JsonCompleted ->
			finalize com;
			0
		| EvalTypes.Sys_exit i | Hlinterp.Sys_exit i ->
			if i <> 0 then com.part_scope.has_error <- true;
			ContextFlush.flush_context com;
			finalize com;
			i

let process_actx com actx =
	com.doinline <- com.display.dms_inline && not (Common.defined com Define.NoInline);
	com.timer_ctx.measure_times <- (if actx.measure_times then Yes else No);
	let check_deprecation_settings () =
		if defined com NoDeprecationWarnings then begin
			com.warning_options <- [{wo_warning = WDeprecated; wo_mode = WMDisable}] :: com.warning_options
		end
	in
	match DisplayProcessing.process_display_arg com actx with
	| Completed ->
		raise DisplayJson.JsonCompleted
	| NeedsTyping ->
		actx.did_something <- true;
		actx.force_typing <- true;
		check_deprecation_settings ()
	| NoCompletionPointFound ->
		check_deprecation_settings ()

let compile_ctx sctx com =
	let run com =
		ServerCache.before_anything sctx com;
		Setup.setup_common_context com;
		compile_safe com (fun () ->
			let actx = Args.process_args com in
			process_actx com actx;
			compile com actx sctx;
		);
		ContextFlush.flush_context com;
		finalize com;
	in
	catch_completion_and_exit com sctx run

let create_context (sctx : ServerCompilationContext.t) request_scope runtime_args has_next =
	let part_scope = {
		runtime_args;
		warned_positions = Hashtbl.create 0;
		has_next;
		has_error = false;
		messages = [];
		report_mode = RMNone;
		compilation_step = sctx.compilation_step;
		pass_debug_messages = DynArray.create ();
		dump_config = DumpConfig.create_default ();
		parser_state = {
			was_auto_triggered = false;
			had_parser_resume = false;
			display_module_has_macro_defines = false;
			delayed_syntax_completion = Atomic.make None;
			special_identifier_files = ThreadSafeHashtbl.create 0;
		};
		file_keys = new file_keys;
		stored_typed_exprs = new Lookup.hashtbl_lookup;
		cached_macros = new Lookup.hashtbl_lookup;
	} in
	Common.create sctx request_scope part_scope (DisplayTypes.DisplayMode.create DMNone)