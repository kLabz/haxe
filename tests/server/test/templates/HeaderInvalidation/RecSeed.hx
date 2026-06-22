class RecSeed {
	public var peer:RecPeer;
	public function new() {}
	public function use():Int {
		return RecPeer.VAL + 1;
	}
}
