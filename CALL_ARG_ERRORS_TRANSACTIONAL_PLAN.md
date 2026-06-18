# Transactional-eager call-arg typing — plan (this branch)

Branch `fix/call-arg-errors-transactional`, based on `fix/call-arg-errors`. Goal: keep the eager
typing of `fix/call-arg-errors` (so type-inference order is unchanged — unlike the deferred POC on
`fix/call-arg-errors-deferred`, which broke hxcoro/utest), but make each *abandoned* speculative
attempt (skipped optional argument, losing overload candidate) **transactional** and replace the
**global** message rollback with **TLazy-safe scoped capture**.

See `CALL_ARG_ERRORS_FINDINGS.md` for why deferral was abandoned.

## Two halves

### 1. Monomorph transaction (concrete, low-risk)

Snapshot/restore each touched monomorph's `tm_type` + `tm_down_constraints` and the
`ctx.e.monomorphs` list around a speculative attempt. There is **working precedent** in
`unify_field_call`'s overload loop (`callUnification.ml`, the `known_monos`/`current_monos` dance):

```
with Error ({ err_message = Call_error _ } as err) ->
    List.iter (fun (m,t,constr) ->
        if t != m.tm_type then m.tm_type <- t;
        if constr != m.tm_down_constraints then m.tm_down_constraints <- constr;
    ) known_monos;
    ctx.e.monomorphs <- current_monos;
```

Factor this into a `Monomorph`/`Typecore` helper (`monomorph_transaction ctx : (unit -> unit)`),
and apply it in `unify_call_args` around the optional-skip attempt (today only messages are rolled
back there, monomorph state leaks).

### 2. TLazy-safe message capture (the hard half — replaces the global rollback)

Today (`fix/call-arg-errors`): `reset/add/rollback_call_arg_body_messages` track entries appended to
the **global** `com.part_scope.messages` and filter them out on a skipped attempt. This is what the
reviewer rejected: a `TLazy` forced mid-attempt commits its (memoized, permanent) errors into that
same global list, and the snapshot-rollback erases them.

Replacement: make message emission interceptable and **re-rooted at lazy/pass boundaries**.

Preferred: **OCaml 5 effects.**
- `type _ Effect.t += Diagnostic : message -> unit Effect.t`; route
  `display_error`/`located_display_error`/`check_error` through `perform`.
- Top-level handler commits to `part_scope`.
- A speculative attempt installs a buffering handler (tap: buffer + immediately resume); commit on
  win, drop on lose.
- **Re-rooting:** `lazy_type` / pass execution installs a handler that forwards straight to the
  permanent sink, so a forced lazy's diagnostics bypass any speculative buffer in the dynamic
  extent. This is the property the global rollback cannot have, made structural in one place.

Fallback if effects are deemed too invasive: a field-local emission sink threaded through the error
API, with the *same* re-rooting discipline at `lazy_type`/pass entry. Less invasive to install but
the reset is a discipline that can be forgotten — which is the class of bug that caused the original
rejection. Effects are preferred precisely because the re-rooting becomes unforgettable.

## Concrete integration points (verified)

- **Emission chokepoints:** `Common.display_error_ext` (`common.ml:1098`) branches to
  `add_diagnostics_message` (`common.ml:1094`, diagnostics mode) or `com.error_ext` (normal mode).
  A com-side capture buffer would be honored here (and in `add_diagnostics_message`).
- **Lazy forcing chokepoint:** `lazy_type` (`tFunctions.ml:348`) — only the `LWait f -> f()` branch
  runs user typing. But `tFunctions` is core and has **no `com`**, so re-rooting can't read the
  capture state directly. Add an indirection hook: `let lazy_force_hook = ref (fun f -> f())` in
  core, called only on the `LWait` branch; the typer installs a hook that clears the active capture
  buffer for the duration of `f()` (so a forced lazy's permanent diagnostics bypass any speculative
  buffer). Keep `LAvailable`/`LProcessing` on the hot path untouched.
- This is the same shape effects would give, done with a com-side buffer stack + one core hook. It
  is performance-sensitive (lazy forcing is hot) and touches core error reporting, so it warrants
  its own focused, well-tested change.

## Order of work

1. [down payment] Monomorph transaction helper + apply at the optional-skip boundary. Build, run
   `tests/misc/eval` + the unit macro target to confirm no regression.
2. Introduce the `Diagnostic` effect and route the three emission entry points through it; top-level
   handler reproduces current behavior (no functional change yet).
3. Buffering handler per speculative attempt; re-rooting handler at `lazy_type`/pass. Remove
   `reset/add/rollback_call_arg_body_messages` and `rollback_messages`.
4. Validate against hxcoro/utest (the code the deferred POC broke) + full server suite.

## Not solved here

Attempt **ranking** when every attempt fails (which buffered attempt to surface — the `MaskStruct`
group-C cases) is a separate, pre-existing rough edge.
