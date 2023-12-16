package types

type Signature struct {
}

func (t *Signature) Underlying() Type { return t }
func (t *Signature) String() string   { return "" }
