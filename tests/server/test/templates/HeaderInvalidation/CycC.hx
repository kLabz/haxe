class CycC {
	static public function use():Int {
		return new CycA().ping();
	}
}
