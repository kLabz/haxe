package unit.issues;

class Issue6195 extends Test {
	static var foo(get, default):Int;
	static function get_foo() return foo;

	function test() {
		foo = 10;
		eq(foo, 10);
	}
}
