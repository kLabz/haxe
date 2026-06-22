class SeedRefVar {
	public static function get():Int {
		return PeerVar.VALUE + PeerVar.calc(1);
	}
}
