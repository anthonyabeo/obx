package sema

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

// InheritanceView builds a program-wide view of record inheritance: it
// detects cycles, computes per-type runtime layouts (including a per-instance
// vptr at offset 0), canonicalizes vtables (storing module-local function
// indices) and prepares RTTI symbol names.  It must run after type-checking
// of all compilation units.
type InheritanceView struct {
	ctx *compiler.Context
}

func NewInheritanceView(ctx *compiler.Context) *InheritanceView {
	return &InheritanceView{ctx: ctx}
}

// RunAll processes all compilation units and computes layouts/vtables for all
// record types found in their module scopes.
func (iv *InheritanceView) RunAll(units []ast.CompilationUnit) {
	// Build a global registry of TypeSymbols -> record types.
	recMap := make(map[*types.RecordType]*ast.TypeSymbol)
	// Keep module name -> function index mapping
	moduleFuncIdx := make(map[string]map[string]uint32)

	// First pass: collect type symbols and build per-module function index
	for _, unit := range units {
		if unit == nil {
			continue
		}
		modName := unit.Name()
		iv.ctx.Env.SetCurrentScope(iv.ctx.Env.ModuleScope(modName))
		// Set ctx.FileName for any diagnostics emitted while scanning this unit
		if modUnit, ok := unit.(*ast.Module); ok {
			iv.ctx.FileName = modUnit.FileName
		} else if defUnit, ok := unit.(*ast.Definition); ok {
			iv.ctx.FileName = defUnit.FileName
		}

		scope := iv.ctx.Env.ModuleScope(modName)
		if scope == nil {
			continue
		}

		// build function index table for this module
		fidx := make(map[string]uint32)
		var next uint32 = 0
		for name, sym := range scope.Elems() {
			if sym == nil {
				continue
			}
			switch sym.Kind() {
			case ast.ProcedureSymbolKind:
				ps := sym.(*ast.ProcedureSymbol)
				m := ps.MangledName()
				if m == "" {
					m = ast.Mangle(ps)
				}
				if _, exists := fidx[m]; !exists {
					fidx[m] = next
					next++
				}
			case ast.TypeSymbolKind:
				ts := sym.(*ast.TypeSymbol)
				// If this type has an AST record with methods in its RecordScope,
				// add them now so methods also receive indices.
				if rec, ok := ts.AstType().(*ast.RecordType); ok && rec.Env != nil {
					for mname, msym := range rec.Env.Methods() {
						if msym == nil {
							continue
						}
						m := msym.MangledName()
						if m == "" {
							m = ast.Mangle(msym)
						}
						if _, exists := fidx[m]; !exists {
							fidx[m] = next
							next++
						}
						_ = mname // keep variable for potential debug
					}
				}
			default:
				_ = name
			}
		}
		moduleFuncIdx[modName] = fidx

		// collect type symbols that denote record types
		for _, sym := range scope.Elems() {
			if sym == nil || sym.Kind() != ast.TypeSymbolKind {
				continue
			}
			ts := sym.(*ast.TypeSymbol)
			// Get sema type and ensure it's a named type whose Def is a record
			ty := ts.Type()
			nt, ok := ty.(*types.NamedType)
			if !ok || nt == nil {
				continue
			}
			rec, ok := types.Underlying(nt.Def).(*types.RecordType)
			if !ok || rec == nil {
				continue
			}
			recMap[rec] = ts
		}
	}

	// Cycle detection + topo ordering over record inheritance graph
	state := make(map[*types.RecordType]int) // 0 unvisited,1 visiting,2 done
	var topo []*types.RecordType
	var stack []*types.RecordType

	var dfs func(r *types.RecordType) bool
	dfs = func(r *types.RecordType) bool {
		if r == nil {
			return true
		}
		if state[r] == 2 {
			return true
		}
		if state[r] == 1 {
			// cycle detected; build cycle list for diagnostics
			var cycle []string
			for i := len(stack) - 1; i >= 0; i-- {
				cycle = append(cycle, fmt.Sprintf("%p", stack[i]))
				if stack[i] == r {
					break
				}
			}
			// Report an error for the named type associated with r if possible
			if ts, ok := recMap[r]; ok {
				iv.ctx.Reporter.Report(diag.Diagnostic{
					Severity: diag.Error,
					Message:  fmt.Sprintf("circular inheritance detected: %v", cycle),
					Range:    iv.ctx.Source.Span(iv.ctx.FileName, ts.AstType().Pos(), ts.AstType().End()),
				})
			} else {
				iv.ctx.Reporter.Report(diag.Diagnostic{
					Severity: diag.Error,
					Message:  "circular inheritance detected",
				})
			}
			return false
		}

		state[r] = 1
		stack = append(stack, r)
		if r.Base != nil {
			if !dfs(r.Base) {
				return false
			}
		}
		// finished
		stack = stack[:len(stack)-1]
		state[r] = 2
		topo = append(topo, r)
		return true
	}

	// Run DFS for all recorded record types
	for r := range recMap {
		if state[r] == 0 {
			if !dfs(r) {
				// continue scanning to report more errors but don't abort
			}
		}
	}

	// Compute layouts in topo order (bases first)
	for i := len(topo) - 1; i >= 0; i-- {
		r := topo[i]
		// find the TypeSymbol for file context
		ts := recMap[r]
		if ts == nil {
			// nothing to report; skip
			continue
		}
		// set file name for diagnostics
		// locate module name via type symbol parent scope
		modName := ""
		if ts.Parent() != nil {
			modName = ts.Parent().Name
		}
		if mScope := iv.ctx.Env.ModuleScope(modName); mScope != nil {
			// If the module has a FileName available through AST units, set ctx.FileName
			// by searching units.  For simplicity we leave ctx.FileName as-is;
		}

		// compute layout
		ptrSize := int(iv.ctx.Target.WordSize)

		var baseLayout *types.RecordLayout
		if r.Base != nil {
			baseLayout = r.Base.Layout
			if baseLayout == nil {
				// Base layout missing — likely error earlier; skip computing this record
				iv.ctx.Reporter.Report(diag.Diagnostic{
					Severity: diag.Warning,
					Message:  "base record layout missing; skipping layout for derived type",
					Range:    iv.ctx.Source.Span(iv.ctx.FileName, ts.AstType().Pos(), ts.AstType().End()),
				})
				continue
			}
		}

		// compute alignment: start with pointer alignment
		recAlign := ptrSize
		// include base alignment
		if baseLayout != nil && baseLayout.Alignment > recAlign {
			recAlign = baseLayout.Alignment
		}
		// include own fields' alignment
		for _, f := range r.Fields {
			if f.Type != nil {
				if a := f.Type.Alignment(); a > recAlign {
					recAlign = a
				}
			}
		}

		// start offset: if has base, start after base.Size else reserve vptr at offset 0
		offset := 0
		if baseLayout != nil {
			offset = baseLayout.Size
			// align to base alignment
			if ba := baseLayout.Alignment; ba > 0 {
				offset = (offset + ba - 1) / ba * ba
			}
		} else {
			// reserve vptr at offset 0
			offset = ptrSize
		}

		fieldOffsets := make(map[string]int)
		fieldLayouts := make([]types.FieldLayout, 0)

		for _, f := range r.Fields {
			// align each field
			fa := 1
			if f.Type != nil {
				fa = f.Type.Alignment()
			}
			if fa > 0 {
				offset = (offset + fa - 1) / fa * fa
			}
			fieldOffsets[f.Name] = offset
			fieldLayouts = append(fieldLayouts, types.FieldLayout{
				Name:   f.Name,
				Type:   f.Type,
				Offset: offset,
				IsPtr:  types.IsPointer(f.Type),
			})
			// advance
			fw := 0
			if f.Type != nil {
				fw = f.Type.Width()
			}
			offset += fw
		}

		// final padding to record alignment
		size := offset
		if recAlign > 0 {
			size = (size + recAlign - 1) / recAlign * recAlign
		}

		rl := &types.RecordLayout{
			FieldOffsets: fieldOffsets,
			Fields:       fieldLayouts,
			Size:         size,
			Alignment:    recAlign,
			VTable:       make([]types.MethodSlot, 0),
			VTableIndex:  make(map[string]int),
		}

		// build vtable: start from base if present
		if baseLayout != nil {
			// copy base entries
			rl.VTable = append(rl.VTable, baseLayout.VTable...)
			for idx, ms := range rl.VTable {
				rl.VTableIndex[ms.Name] = idx
			}
		}

		// find ast record to read method symbols
		astRec, _ := ts.AstType().(*ast.RecordType)
		if astRec != nil && astRec.Env != nil {
			methods := astRec.Env.Methods()
			for mname, msym := range methods {
				if msym == nil {
					continue
				}
				psym, ok := msym.(*ast.ProcedureSymbol)
				if !ok {
					continue
				}

				// perform override signature checks against base methods
				if r.Base != nil {
					if baseMethod := r.Base.GetMethod(mname); baseMethod != nil {
						pOrig, ok1 := baseMethod.Type.(*types.ProcedureType)
						pRedef := psym.Type().(*types.ProcedureType)
						if !ok1 || pRedef == nil || !types.FormalParamsListMatch(pOrig.Params, pRedef.Params) {
							iv.ctx.Reporter.Report(diag.Diagnostic{
								Severity: diag.Error,
								Message:  fmt.Sprintf("parameter mismatch in redefinition of method '%s'", mname),
								Range:    iv.ctx.Source.Span(iv.ctx.FileName, ts.AstType().Pos(), ts.AstType().End()),
							})
						}
						// Check result type match
						if !types.ResultTypeMatch(pOrig.Result, pRedef.Result) {
							iv.ctx.Reporter.Report(diag.Diagnostic{
								Severity: diag.Error,
								Message:  fmt.Sprintf("result type mismatch in redefinition of method '%s'", mname),
								Range:    iv.ctx.Source.Span(iv.ctx.FileName, ts.AstType().Pos(), ts.AstType().End()),
							})
						}
					}
				}

				// compute mangled name and function index via module table
				mangled := psym.MangledName()
				if mangled == "" {
					mangled = ast.Mangle(psym)
				}
				// locate function index table for the module
				fidx := moduleFuncIdx[modName]
				var findex uint32 = 0
				if fidx != nil {
					if idx, ok := fidx[mangled]; ok {
						findex = idx
					} else {
						// If missing, assign a new index (shouldn't normally happen)
						n := uint32(len(fidx))
						fidx[mangled] = n
						findex = n
					}
				}

				if idx, exists := rl.VTableIndex[mname]; exists {
					// override: replace slot
					rl.VTable[idx] = types.MethodSlot{Name: mname, Mangled: mangled, FuncIndex: findex}
				} else {
					// append new slot
					rl.VTableIndex[mname] = len(rl.VTable)
					rl.VTable = append(rl.VTable, types.MethodSlot{Name: mname, Mangled: mangled, FuncIndex: findex})
				}
			}
		}

		// set RTTI/VTable symbol names using module/type naming
		// use simple mangling: Module$Type$rtti and Module$Type$vtab
		typeName := ts.Name()
		if modName == "" {
			rl.RTTIName = fmt.Sprintf("%s$rtti", typeName)
			rl.VTableName = fmt.Sprintf("%s$vtab", typeName)
		} else {
			rl.RTTIName = fmt.Sprintf("%s$%s$rtti", modName, typeName)
			rl.VTableName = fmt.Sprintf("%s$%s$vtab", modName, typeName)
		}

		r.Layout = rl
	}
}
