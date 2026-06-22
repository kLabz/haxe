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
}
