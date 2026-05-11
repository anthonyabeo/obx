package mir

// AddBlockAndSetEntry is a small convenience for constructing linear test
// graphs without wiring the block twice.
func (fn *Function) AddBlockAndSetEntry(b *Block) {
	if fn == nil || b == nil {
		return
	}
	fn.AddBlock(b)
	if fn.Entry == nil {
		fn.Entry = b
	}
}
