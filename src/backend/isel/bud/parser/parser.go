package parser

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/backend/isel/bud/ast"
)

type Parser struct {
	lx  *Lexer
	cur Token
	nxt Token
}

func NewParser(lx *Lexer) *Parser {
	p := &Parser{lx: lx}
	p.cur = lx.NextToken()
	p.nxt = lx.NextToken()
	return p
}

func (p *Parser) next() {
	p.cur = p.nxt
	p.nxt = p.lx.NextToken()
}

func (p *Parser) matchAny(tt ...TokenType) string {
	for _, t := range tt {
		if p.cur.Kind == t {
			val := p.cur.Value
			p.next()
			return val
		}
	}

	panic(fmt.Sprintf("expected on of %v, got %v", tt, p.cur.Value))
}

func (p *Parser) match(tt TokenType) string {
	if p.cur.Kind != tt {
		panic(fmt.Sprintf("expected %v, got %v (%s)", tt, p.cur.Kind, p.cur.Value))
	}
	val := p.cur.Value
	p.next()
	return val
}

func (p *Parser) peek() Token {
	return p.nxt
}

func (p *Parser) Parse() *ast.MachineDesc {
	md := &ast.MachineDesc{}

	if p.cur.Kind == TokHeader {
		md.Header = p.parseHeader()
	}

	for p.cur.Kind != TokEOF {
		md.Rules = append(md.Rules, p.parseRule())
	}

	return md
}

func (p *Parser) parseHeader() *ast.Header {
	h := &ast.Header{Fields: make(map[string][]string)}

	p.match(TokHeader)
	p.match(TokLBrace)

	for p.cur.Value != "}" && p.cur.Kind != TokEOF {
		keyTok := p.match(TokIdent)
		p.match(TokColon)
		valTok := p.peek()
		switch valTok.Kind {
		case TokIdent, TokString, TokInt:
			p.next()
			h.Fields[keyTok] = append(h.Fields[keyTok], valTok.Value)
		default:
			panic(fmt.Sprintf("unexpected value '%s'in header", valTok.Value))
		}
	}

	p.match(TokRParen)
	return h
}

func (p *Parser) parseRule() *ast.Rule {
	p.match(TokRule)
	name := p.match(TokIdent)
	p.match(TokLBrace)

	r := &ast.Rule{Name: name}

	for {
		switch p.cur.Kind {
		case TokOut:
			p.next()
			r.Out = p.parseOperand()
			p.match(TokSemi)
		case TokIn:
			p.next()
			r.In = p.parseOperandList()
			p.match(TokSemi)
		case TokTemps:
			p.next()
			r.Temps = p.parseOperandList()
			p.match(TokSemi)
		case TokPattern:
			p.next()
			r.Pattern = p.parsePattern()
			p.match(TokSemi)
		case TokEmit:
			p.next()
			r.Instructions = p.parseEmitBlock()
		case TokCost:
			p.next()
			value := p.match(TokInt)
			n, err := strconv.Atoi(value)
			if err != nil {
				panic("invalid number: " + value)
			}

			r.Cost = n
			p.match(TokSemi)
		case TokCond:
			p.next()
			r.Predicates = p.parseCondList()
			p.match(TokSemi)
		case TokRBrace:
			p.next()
			return r
		default:
			panic("unexpected token in rule: " + p.cur.Value)
		}
	}
}

func (p *Parser) parseOperand() ast.Operand {
	var operand ast.Operand

	switch p.cur.Kind {
	case TokGPR, TokSPR, TokFPR:
		operand = p.parseReg()
	case TokImm:
		operand = p.parseImm()
	case TokReloc:
		operand = p.parseReloc()
	case TokMem:
		operand = p.parseMem()
	case TokLabel:
		operand = p.parseLabel()
	case TokSymbol:
		operand = p.parseSymbol()
	case TokArg:
		operand = p.parseArg()
	default:
		panic("unexpected token in operand: " + p.cur.Value)
	}

	return operand
}

func (p *Parser) parseArg() *ast.Arg {
	p.match(TokArg)
	p.match(TokLBrace)
	imm := p.parseImm()
	p.match(TokRBrace)
	return &ast.Arg{Index: imm}
}

func (p *Parser) parseSymbol() *ast.Symbol {
	p.match(TokSymbol)
	p.match(TokColon)
	name := p.matchIdent()
	return &ast.Symbol{Name: name}
}

func (p *Parser) parseImm() *ast.Imm {
	p.match(TokImm)
	p.match(TokColon)
	if p.cur.Kind == TokDollar {
		p.next()
		name := p.match(TokIdent)
		return &ast.Imm{Name: name}
	}
	return &ast.Imm{Value: p.parseNumber()}
}

func (p *Parser) parseReg() *ast.Register {
	reg := p.matchAny(TokFPR, TokGPR, TokSPR)
	p.match(TokColon)
	mode := p.matchAny(TokVirt, TokPhys)
	p.match(TokColon)
	if p.cur.Kind == TokDollar {
		p.next()
	}
	name := p.match(TokIdent)
	return &ast.Register{Name: name, Type: reg, Mode: mode}
}

func (p *Parser) parseLabel() *ast.Label {
	var value string

	p.match(TokLabel)
	p.match(TokColon)
	name := p.matchIdent()
	if p.cur.Kind == TokEqual {
		p.next()
		value = p.match(TokString)
	}
	return &ast.Label{Name: name, Value: value}
}

func (p *Parser) parseMem() *ast.Mem {
	p.match(TokMem)
	p.match(TokColon)
	p.match(TokLBrace)

	p.match(TokBase)
	p.match(TokEqual)
	base := p.parseReg()

	p.match(TokComma)

	p.match(TokOffset)
	p.match(TokEqual)

	var offs ast.Operand
	switch p.cur.Kind {
	case TokReloc:
		offs = p.parseReloc()
	case TokImm:
		offs = p.parseImm()
	default:
		panic("unexpected token in mem: " + p.cur.Value)
	}

	p.match(TokRBrace)

	return &ast.Mem{Base: base, Offs: offs}
}

func (p *Parser) parseReloc() *ast.Reloc {
	p.match(TokReloc)
	p.match(TokColon)
	fxn := p.match(TokIdent)
	p.match(TokLParen)
	symbol := p.matchIdent()
	p.match(TokRParen)

	return &ast.Reloc{Fxn: fxn, Symbol: symbol}
}

func (p *Parser) parseOperandList() (list []ast.Operand) {
	list = append(list, p.parseOperand())
	for p.cur.Kind == TokComma {
		p.next()
		list = append(list, p.parseOperand())
	}

	return
}

func (p *Parser) parseNumber() any {
	var neg bool
	if p.cur.Kind == TokSub {
		p.next()
		neg = true
	}

	value := p.match(TokInt)

	n, err := strconv.Atoi(value)
	if err != nil {
		panic("invalid number: " + value)
	}

	if neg {
		return -n
	}

	return n
}

func (p *Parser) matchIdent() string {
	if p.cur.Kind == TokDollar {
		p.next()
	}

	return p.match(TokIdent)
}

func (p *Parser) parsePattern() *ast.Pattern {
	op := p.matchIdent()
	if p.cur.Kind == TokLParen {
		p.next()
		var args []*ast.Pattern
		for {
			args = append(args, p.parsePattern())
			if p.cur.Kind == TokComma {
				p.next()
				continue
			}
			break
		}
		p.match(TokRParen)
		return &ast.Pattern{Op: op, Args: args}
	}
	return &ast.Pattern{Op: op, Var: op}
}

func (p *Parser) parseEmitBlock() []ast.Instr {
	var lines []ast.Instr
	p.match(TokLBrace)
	for {
		if p.cur.Kind == TokRBrace {
			p.next()
			break
		}

		inst := p.parseInstr()
		lines = append(lines, inst)
		if p.cur.Kind == TokSemi {
			p.next()
		}
	}
	return lines
}

func (p *Parser) parseInstr() ast.Instr {
	p.match(TokInstr)
	p.match(TokLBrace)

	var (
		opcode   string
		operands []ast.Operand
		def      *ast.Register
		uses     []*ast.Register
	)

	for {
		if p.cur.Kind == TokRBrace {
			p.next()
			break
		}

		p.match(TokOpcode)
		p.match(TokColon)
		opcode = p.match(TokString)

		if p.cur.Kind == TokComma {
			p.next()
		}

		if p.cur.Kind == TokOperands {
			p.next()
			p.match(TokColon)
			p.match(TokLBrack)

			operands = p.parseOperandList()
			p.match(TokRBrack)
		}

		if p.cur.Kind == TokComma {
			p.next()
		}

		if p.cur.Kind == TokDef {
			p.next()
			p.match(TokColon)
			def = p.parseReg()
		}

		if p.cur.Kind == TokComma {
			p.next()
		}

		if p.cur.Kind == TokUses {
			p.next()
			p.match(TokColon)
			p.match(TokLBrack)

			for {
				uses = append(uses, p.parseReg())
				if p.cur.Kind == TokComma {
					p.next()
					continue
				}
				break
			}

			p.match(TokRBrack)
		}
	}

	return ast.Instr{
		Opcode:   opcode,
		Operands: operands,
		Def:      def,
		Uses:     uses,
	}
}

func (p *Parser) parseCondList() []*ast.Predicate {
	var conds []*ast.Predicate
	for {
		neg := false
		if p.cur.Kind == TokBang {
			neg = true
			p.next()
		}
		name := p.match(TokIdent)
		c := &ast.Predicate{Name: name, Negated: neg}
		if p.cur.Kind == TokLParen {
			p.next()
			for {
				c.Args = append(c.Args, p.matchIdent())
				if p.cur.Kind == TokComma {
					p.next()
					continue
				}
				break
			}
			p.match(TokRParen)
		}
		conds = append(conds, c)

		if p.cur.Kind == TokComma {
			p.next()
			continue
		}
		break
	}
	return conds
}
