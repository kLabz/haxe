class GUser {
	public var seed:GSeed;
	public static function use():Int {
		var m = new GMap<String,Int>();
		return m.get("x") + GSeed.v();
	}
}
