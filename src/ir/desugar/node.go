package desugar

// NodeBase provides embedded Start/End source offsets for all HIR nodes.
// Embedding this struct in node types exposes Start and End fields directly
// (e.g. node.Start) without requiring each node to declare them.
type NodeBase struct {
    Start int
    End   int
}

