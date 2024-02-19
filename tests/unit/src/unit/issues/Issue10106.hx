package unit.issues;

class Issue10106CExtension {
	public static function fromS(cls:Class<Issue10106C>, s:String) {
		return new Issue10106C(s);
	}
}

@:using(unit.issues.Issue10106.Issue10106CExtension)
class Issue10106C {
	public final s:String;

	public function new(s:String) {
		this.s = s;
	}
}

class Issue10106EnExtension {
	public static function fromS(en:Enum<Issue10106En>, st:String):Issue10106En {
		return A;
	}
}

@:using(unit.issues.Issue10106.Issue10106EnExtension)
enum Issue10106En {
	A;
	B;
}

class Issue10106 extends Test {
	function test() {
		eq(A, Issue10106En.fromS("A"));
		eq("foo", Issue10106C.fromS("foo").s);
	}
}
