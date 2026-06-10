class DepGeneric {
	@:generic public static function tag<T>(v:T):String {
		return "1:" + v;
	}
}
