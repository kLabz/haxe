extern class Api {
	overload static function take(f:Int->Int):Void;
	overload static function take(n:Int):Void;
}

function main() {
	// The lambda matches the (Int->Int) overload; its body error must be reported in
	// place, WITHOUT a spurious "For function argument" wrapper (it is a body error, not a
	// signature unification).
	//
	// STILL DISABLED after the scoped-capture work. The non-overload path now surfaces body
	// errors cleanly via the capture buffer, but in overloads body errors propagate so a
	// candidate fails (rather than being captured) -- on purpose, since a body error must be
	// able to disqualify a candidate during selection. The body error therefore reaches
	// arg_error and picks up the "For function argument" wrapper. Reporting it cleanly needs
	// overload-failure *ranking* (surface the structurally-best candidate's body error without
	// the wrapper), which is the deferred problem noted in CALL_ARG_ERRORS_TRANSACTIONAL_PLAN.md.
	Api.take(x -> {
		undefinedThing;
	});
}
