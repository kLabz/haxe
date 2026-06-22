class MainLeak {
	static public function main() {
		trace(SeedRefPeer.get());
		trace(PeerInline.value());
	}
}
