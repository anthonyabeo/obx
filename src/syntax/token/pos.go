package token

import "fmt"

type Pos struct {
	FileName string
	Line     int
	Start    int
	End      int
}

func NewTextRange(fileName string, line, start, end int) Pos {
	return Pos{
		FileName: fileName,
		Line:     line,
		Start:    start,
		End:      end,
	}
}

func (r *Pos) String() string {
	s := r.FileName
	if r.FileName != "" {
		s += fmt.Sprintf(":%d:%d..%d", r.Line, r.Start, r.End)
	} else {
		s = "-"
	}

	return s
}
