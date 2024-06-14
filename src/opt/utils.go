package opt

import "github.com/anthonyabeo/obx/src/translate/ir"

// BlockValuePair
// ---------------------------------------------------------------
type BlockValuePair struct {
	blk *ir.BasicBlock
	val ir.Value
}

// Stack
// ---------------------------------------------------------------
type Stack struct {
	data []map[string]*BlockValuePair
}

func (s *Stack) Push(tup map[string]*BlockValuePair) {
	s.data = append(s.data, tup)
}

func (s *Stack) Pop() map[string]*BlockValuePair {
	item := s.data[len(s.data)-1]
	s.data = s.data[:len(s.data)-1]

	return item
}

func (s *Stack) Top() map[string]*BlockValuePair {
	if len(s.data) > 0 {
		return s.data[len(s.data)-1]
	}
	return nil
}

func (s *Stack) Size() int { return len(s.data) }

func (s *Stack) BlockValuePair(f string) (*ir.BasicBlock, ir.Value) {
	for i := s.Size() - 1; i >= 0; i-- {
		frame := s.data[i]
		if tuple, ok := frame[f]; ok {
			return tuple.blk, tuple.val
		}
	}

	return nil, nil
}
