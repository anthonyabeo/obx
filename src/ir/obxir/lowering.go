package obxir

// ExpandGEP is a pre-codegen lowering pass that replaces every GEPInst with
// the equivalent sequence of BinaryInst (ADD / MUL) instructions.
//
// GEPInst is the optimized IR representation used during analysis and
// optimization passes.  Before instruction selection, it is expanded into
// primitive arithmetic the target-neutral isel patterns already understand:
//
//	Record field access:
//	  t := GEP base .field
//	  →
//	  t := ADD base, <fieldOffset>   ; t.IsAddr=true
//
//	Array element access:
//	  t := GEP base [idx]
//	  →
//	  tScale := MUL idx, <elemBytes>
//	  t := ADD base, tScale          ; t.IsAddr=true
//
// Multi-step chains are expanded left-to-right, accumulating the running
// address pointer at each step.
func ExpandGEP(fn *Function) {
	for _, blk := range fn.SortedBlocks() {
		expanded := make([]Instr, 0, len(blk.Instrs))
		for _, inst := range blk.Instrs {
			gep, ok := inst.(*GEPInst)
			if !ok {
				expanded = append(expanded, inst)
				continue
			}
			// cur tracks the current address value as we process indices.
			cur := gep.Base
			curType := gep.ElemType

			for k, idx := range gep.Indices {
				isLast := k == len(gep.Indices)-1

				if idx.Field != "" {
					// ── Record field step ────────────────────────────────────
					recType, ok := curType.(*RecordType)
					if !ok {
						panic("ExpandGEP: GEPIndex.Field on non-record type")
					}
					field, exists := recType.Field(idx.Field)
					if !exists {
						panic("ExpandGEP: field not found: " + idx.Field)
					}
					offset := Int64Lit(uint64(field.Offset))

					var addTarget *Temp
					if isLast {
						addTarget = gep.Target
					} else {
						addTarget = &Temp{
							Ident:    gep.Target.Ident + "_step",
							OrigName: gep.Target.OrigName,
							Typ:      Int64Type,
							IsAddr:   true,
						}
					}
					expanded = append(expanded, &BinaryInst{
						Target: addTarget,
						Op:     ADD,
						Left:   cur,
						Right:  offset,
					})
					cur = addTarget
					curType = field.Type

				} else if idx.Index != nil {
					// ── Array/pointer element step ───────────────────────────
					var elemSize int
					switch et := curType.(type) {
					case *ArrayType:
						elemSize = et.Elem.Width()
						curType = et.Elem
					case *PointerType:
						if et.Ref != nil {
							// Descend into MultiDim arrays if needed.
							if at, ok := et.Ref.(*ArrayType); ok {
								elemSize = at.Elem.Width()
								curType = at.Elem
							} else {
								elemSize = et.Ref.Width()
								curType = et.Ref
							}
						} else {
							elemSize = 1 // void* → byte stride
							curType = UInt8Type
						}
					default:
						panic("ExpandGEP: GEPIndex.Index on non-array/pointer type")
					}

					stride := Int64Lit(uint64(elemSize))

					// tScale = idx * elemSize
					scaleTarget := &Temp{
						Ident:    gep.Target.Ident + "_scale",
						OrigName: gep.Target.OrigName,
						Typ:      Int64Type,
						IsAddr:   false,
					}
					expanded = append(expanded, &BinaryInst{
						Target: scaleTarget,
						Op:     MUL,
						Left:   idx.Index,
						Right:  stride,
					})

					var addTarget *Temp
					if isLast {
						addTarget = gep.Target
					} else {
						addTarget = &Temp{
							Ident:    gep.Target.Ident + "_step",
							OrigName: gep.Target.OrigName,
							Typ:      Int64Type,
							IsAddr:   true,
						}
					}
					expanded = append(expanded, &BinaryInst{
						Target: addTarget,
						Op:     ADD,
						Left:   cur,
						Right:  scaleTarget,
					})
					cur = addTarget
				}
			}
		}
		blk.Instrs = expanded
	}
}

