class EADep {
	public static function f(x:EAKind = EAKind.Any):Int {
		return EASeed.v() == x ? 1 : 0;
	}
}
