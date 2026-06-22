abstract GMap<K,V>(Int) {
	public inline function new() this = 0;
	public inline function get(k:K):V return cast this;
}
