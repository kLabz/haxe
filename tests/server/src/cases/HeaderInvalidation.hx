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
		assertReuse("Main");

		// Signature change: Dep.value now returns Float -> Main must be re-typed (not reused).
		vfs.putContent("Dep.hx", getTemplate("HeaderInvalidation/Dep.hx").replace("value():Int", "value():Float"));
		runHaxeJson([], ServerMethods.Invalidate, {file: new FsPath("Dep.hx")});
		runHaxe(args);
		Assert.isFalse(hasMessage("reusing Main"));
	}
}
