package dsl

import (
	"fmt"
	"strconv"
)

type Parser struct {
	lx   *Lexer
	cur  Token
	next Token
}

func NewParser(lx *Lexer) *Parser {
	p := &Parser{lx: lx}
	p.cur = lx.NextToken()
	p.next = lx.NextToken()
	return p
}

func (p *Parser) bump() {
	p.cur = p.next
	p.next = p.lx.NextToken()
}

func (p *Parser) peek() TokenType {
	return p.next.Kind
}

func (p *Parser) expect(tt TokenType) string {
	if p.cur.Kind != tt {
		panic(fmt.Sprintf("expected %v, got %v (%s)", tt, p.cur.Kind, p.cur.Value))
	}
	val := p.cur.Value
	p.bump()
	return val
}

func (p *Parser) expectIdent() string {
	if p.cur.Kind == TokDollar {
		p.bump()
	}
	tok := p.expect(TokIdent)
	return tok
}

func (p *Parser) expectNumber() int {
	tok := p.expect(TokInt)
	n, err := strconv.Atoi(tok)
	if err != nil {
		panic("invalid number: " + tok)
	}
	return n
}

func (p *Parser) expectString() string {
	tok := p.expect(TokString)
	// Remove surrounding quotes if your lexer doesn’t already
	if len(tok) >= 2 && tok[0] == '"' && tok[len(tok)-1] == '"' {
		return tok[1 : len(tok)-1]
	}
	return tok
}

func (p *Parser) Parse() []*Rule {
	var rules []*Rule
	for p.cur.Kind != TokEOF {
		rules = append(rules, p.parseRule())
	}

	return rules
}

func (p *Parser) parseRule() *Rule {
	p.expect(TokRule) // "rule"
	name := p.expectIdent()
	p.expect(TokLBrace)

	r := &Rule{Name: name}

	for {
		switch p.cur.Kind {
		case TokOut:
			p.bump()
			r.Out = p.parseVar()
			p.expect(TokSemi)
		case TokIn:
			p.bump()
			r.In = p.parseVarList()
			p.expect(TokSemi)
		case TokTemps:
			p.bump()
			r.Temps = p.parseVarList()
			p.expect(TokSemi)
		case TokPattern:
			p.bump()
			r.Pattern = p.parseExpr()
			p.expect(TokSemi)
		case TokAsm:
			p.bump()
			r.Instructions = p.parseAsmBlock()
		case TokCost:
			p.bump()
			r.Cost = p.expectNumber()
			p.expect(TokSemi)
		case TokCond:
			p.bump()
			r.Predicates = p.parseCondList()
			p.expect(TokSemi)
		case TokRBrace:
			p.bump()
			return r
		default:
			panic("unexpected token in rule: " + p.cur.Value)
		}
	}
}

func (p *Parser) parseVar() Var {
	kind := p.expectIdent()
	p.expect(TokColon)
	p.expect(TokDollar)
	name := p.expectIdent()

	return Var{Name: name, Kind: kind}
}

func (p *Parser) parseVarList() []Var {
	var vars []Var
	for {
		vars = append(vars, p.parseVar())
		if p.cur.Kind == TokComma {
			p.bump()
			continue
		}
		break
	}
	return vars
}

func (p *Parser) parseExpr() *Expr {
	op := p.expectIdent()
	if p.cur.Kind == TokLParen {
		p.bump()
		var args []*Expr
		for {
			args = append(args, p.parseExpr())
			if p.cur.Kind == TokComma {
				p.bump()
				continue
			}
			break
		}
		p.expect(TokRParen)
		return &Expr{Op: op, Args: args}
	}
	return &Expr{Op: op, Var: op}
}

func (p *Parser) parseAsmBlock() []string {
	var lines []string
	p.expect(TokLBrace)
	for {
		if p.cur.Kind == TokRBrace {
			p.bump()
			break
		}
		line := p.expectString()
		lines = append(lines, line)
		if p.cur.Kind == TokSemi {
			p.bump()
		}
	}
	return lines
}

func (p *Parser) parseCondList() []Predicate {
	var conds []Predicate
	for {
		neg := false
		if p.cur.Kind == TokBang {
			neg = true
			p.bump()
		}
		name := p.expectIdent()
		c := Predicate{Name: name, Negated: neg}
		if p.cur.Kind == TokLParen {
			p.bump()
			for {
				c.Args = append(c.Args, p.expectIdent())
				if p.cur.Kind == TokComma {
					p.bump()
					continue
				}
				break
			}
			p.expect(TokRParen)
		}
		conds = append(conds, c)

		if p.cur.Kind == TokComma {
			p.bump()
			continue
		}
		break
	}
	return conds
}
