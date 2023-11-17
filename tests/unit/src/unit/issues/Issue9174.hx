package unit.issues;

class Issue9174 extends unit.Test {
	function test() {
		var result = false;
		try {
			try {
				throw '';
			} catch(e:String) {
				result = true;
				eq(true, result);
				throw e;
			}
		} catch(e:String) {
			eq(true, result);
		}
	}
}
