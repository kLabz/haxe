package cases;

import haxe.display.FsPath;
import haxe.display.Server;
import utest.Assert;

using StringTools;

class HeaderInvalidation extends TestCase {
	// Editing a dependency's method *body* (signature unchanged) must spare its dependents,
	// while editing the *signature* must invalidate them. Opt-in via -D hxb.header-invalidation.
	function testBodyVsSignature() {
		vfs.putContent("Dep.hx", getTemplate("HeaderInvalidation/Dep.hx"));
		vfs.putContent("Main.hx", getTemplate("HeaderInvalidation/Main.hx"));
		var args = ["-main", "Main", "--no-output", "-js", "no.js", "-D", "hxb.header-invalidation"];
		runHaxe(args);

		// Body-only change: Dep is recompiled, but Main's used signature is unchanged -> reused.
		vfs.putContent("Dep.hx", getTemplate("HeaderInvalidation/Dep.hx").replace("return 1", "return 2"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dep.hx")});
		runHaxe(args);
		assertSuccess();
		assertReuse("Main");

		// Signature change: Dep.value now returns Float -> Main must be re-typed (not reused).
		vfs.putContent("Dep.hx", getTemplate("HeaderInvalidation/Dep.hx").replace("value():Int", "value():Float"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dep.hx")});
		runHaxe(args);
		assertSuccess();
		Assert.isFalse(hasMessage("reusing Main"));
	}

	// Inlined bodies are baked into callers, so changing an inline function's *body* (signature
	// unchanged) must still invalidate the caller, even though the header is identical.
	function testInlineBody() {
		vfs.putContent("DepInline.hx", getTemplate("HeaderInvalidation/DepInline.hx"));
		vfs.putContent("MainInline.hx", getTemplate("HeaderInvalidation/MainInline.hx"));
		var args = ["-main", "MainInline", "--no-output", "-js", "no.js", "-D", "hxb.header-invalidation"];
		runHaxe(args);

		// Content-free invalidate of an inline-field module: the impl-field body rendering must be
		// stable across compiles, so the header is identical and the caller is spared.
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("DepInline.hx")});
		runHaxe(args);
		assertSuccess();
		assertReuse("MainInline");

		vfs.putContent("DepInline.hx", getTemplate("HeaderInvalidation/DepInline.hx").replace("return 1", "return 2"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("DepInline.hx")});
		runHaxe(args);
		assertSuccess();
		Assert.isFalse(hasMessage("reusing MainInline"));
	}

	// @:generic functions are specialized into callers, so a body change (signature unchanged) must
	// invalidate the call site even though the header signature is identical.
	function testGenericBody() {
		vfs.putContent("DepGeneric.hx", getTemplate("HeaderInvalidation/DepGeneric.hx"));
		vfs.putContent("MainGeneric.hx", getTemplate("HeaderInvalidation/MainGeneric.hx"));
		var args = ["-main", "MainGeneric", "--no-output", "-js", "no.js", "-D", "hxb.header-invalidation"];
		runHaxe(args);

		vfs.putContent("DepGeneric.hx", getTemplate("HeaderInvalidation/DepGeneric.hx").replace('"1:"', '"2:"'));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("DepGeneric.hx")});
		runHaxe(args);
		assertSuccess();
		Assert.isFalse(hasMessage("reusing MainGeneric"));
	}

	// Partial-peer leak repro: re-typing an edited seed in the isolated pre-phase partial-restores a
	// CLEAN inline peer (cf_expr deferred -> None). That partial object must NOT leak into the main
	// compile, where a re-typed dependent inlines the same peer. If it leaks, inlining a cf_expr=None
	// field fails ("Recursive inline is not supported").
	function testPartialPeerLeak() {
		vfs.putContent("PeerInline.hx", getTemplate("HeaderInvalidation/PeerInline.hx"));
		vfs.putContent("SeedRefPeer.hx", getTemplate("HeaderInvalidation/SeedRefPeer.hx"));
		vfs.putContent("MainLeak.hx", getTemplate("HeaderInvalidation/MainLeak.hx"));
		var args = ["-main", "MainLeak", "--no-output", "-js", "no.js", "-D", "hxb.header-invalidation", "-D", "hxb.prephase-isolate", "-D", "hxb.prephase-partial"];
		runHaxe(args);
		assertSuccess();

		// Signature change on the seed: MainLeak (depends on SeedRefPeer's signature) must be re-typed,
		// and it inlines PeerInline -> exercises the leaked partial peer if any.
		vfs.putContent("SeedRefPeer.hx", getTemplate("HeaderInvalidation/SeedRefPeer.hx").replace("get():Int", "get():Float"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("SeedRefPeer.hx")});
		runHaxe(args);
		assertSuccess();
	}

	// As above but the clean peer exposes an inline VAR (and an inline function using it). Inlining a
	// var whose body was deferred to None takes the acc_get `Var _,None` path and RAISES "Recursive
	// inline is not supported" -- which the typer's error recovery records via com.error_ext WITHOUT
	// re-raising, so it bypasses the pre-phase try/with and reaches the user unless the pre-phase mutes
	// its diagnostics. This case fails (assertSuccess) without that muting.
	function testPartialPeerLeakInlineVar() {
		vfs.putContent("PeerVar.hx", getTemplate("HeaderInvalidation/PeerVar.hx"));
		vfs.putContent("SeedRefVar.hx", getTemplate("HeaderInvalidation/SeedRefVar.hx"));
		vfs.putContent("MainVar.hx", getTemplate("HeaderInvalidation/MainVar.hx"));
		var args = ["-main", "MainVar", "--no-output", "-js", "no.js", "-D", "hxb.header-invalidation", "-D", "hxb.prephase-isolate", "-D", "hxb.prephase-partial"];
		runHaxe(args);
		assertSuccess();

		vfs.putContent("SeedRefVar.hx", getTemplate("HeaderInvalidation/SeedRefVar.hx").replace("get():Int", "get():Float"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("SeedRefVar.hx")});
		runHaxe(args);
		assertSuccess();
	}

	// Increment 2 (-D hxb.prephase-partial-dirty): editing a seed inside a dependency cycle. CycA<->CycB
	// are mutually recursive (an SCC); CycC depends on CycA from OUTSIDE the cycle. Editing CycA's body
	// makes CycB dirty *only by dependency* (its own source is unchanged). With partial-dirty the
	// pre-phase restores CycB signature-only to compute CycA's header instead of re-typing the whole SCC.
	// This must stay sound: a body edit spares CycC, a signature edit invalidates it, both build cleanly
	// (guards against the partial-restore-of-a-cyclic-peer crash / stale-header classes).
	function testCyclicPeerPartialDirty() {
		vfs.putContent("CycA.hx", getTemplate("HeaderInvalidation/CycA.hx"));
		vfs.putContent("CycB.hx", getTemplate("HeaderInvalidation/CycB.hx"));
		vfs.putContent("CycC.hx", getTemplate("HeaderInvalidation/CycC.hx"));
		vfs.putContent("CycMain.hx", getTemplate("HeaderInvalidation/CycMain.hx"));
		var args = ["-main", "CycMain", "--no-output", "-js", "no.js", "-D", "hxb.header-invalidation",
			"-D", "hxb.prephase-isolate", "-D", "hxb.prephase-partial", "-D", "hxb.prephase-partial-dirty"];
		runHaxe(args);
		assertSuccess();

		// Body-only edit of CycA (signature unchanged): CycC's used signature is unchanged -> spared.
		vfs.putContent("CycA.hx", getTemplate("HeaderInvalidation/CycA.hx").replace("return 1", "return 2"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("CycA.hx")});
		runHaxe(args);
		assertSuccess();
		assertReuse("CycC");

		// Signature edit of CycA.ping (extra defaulted arg keeps CycC's call valid): CycC depends on
		// ping's signature -> it must be re-typed, not reused.
		vfs.putContent("CycA.hx", getTemplate("HeaderInvalidation/CycA.hx").replace("ping():Int", "ping(extra:Int = 0):Int"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("CycA.hx")});
		runHaxe(args);
		assertSuccess();
		Assert.isFalse(hasMessage("reusing CycC"));
	}
}
