class Main {
	public function new() {
		var arr:Array<Foo> = [];
		shuffleArray(arr);
	}

	public function shuffleArray<T>(arr:Array<T>):Void {
		//do something with array
	}
}

@:nativeGen
class Foo {
	public function new() { }
}
