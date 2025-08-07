package pprint

// FindModule returns the first module node in the top-level AST JSON.
func FindModule(ast map[string]any) map[string]any {
	units, ok := ast["units"].([]any)
	if !ok || len(units) == 0 {
		return nil
	}
	for _, unit := range units {
		if mod, ok := unit.(map[string]any); ok && mod["type"] == "Module" {
			return mod
		}
	}
	return nil
}

// FindProcedureByName returns a ProcedureDecl node by name (e.g. "Init").
func FindProcedureByName(ast map[string]any, name string) map[string]any {
	mod := FindModule(ast)
	if mod == nil {
		return nil
	}
	decls, ok := mod["declarations"].([]any)
	if !ok {
		return nil
	}
	for _, d := range decls {
		decl, _ := d.(map[string]any)
		if decl["type"] != "ProcedureDecl" {
			continue
		}
		heading, _ := decl["heading"].(map[string]any)
		nameNode, _ := heading["name"].(map[string]any)
		if nameStr, ok := nameNode["name"].(string); ok && nameStr == name {
			return decl
		}
	}
	return nil
}

// FindVariableByName returns a VariableDecl by name (e.g. "x").
func FindVariableByName(ast map[string]any, name string) map[string]any {
	mod := FindModule(ast)
	if mod == nil {
		return nil
	}
	decls, ok := mod["declarations"].([]any)
	if !ok {
		return nil
	}
	for _, d := range decls {
		decl, _ := d.(map[string]any)
		if decl["type"] != "VariableDecl" {
			continue
		}
		idents, _ := decl["names"].([]any)
		for _, identRaw := range idents {
			ident, _ := identRaw.(map[string]any)
			if ident["name"] == name {
				return decl
			}
		}
	}
	return nil
}

// FindConstantByName returns a ConstantDecl by name (e.g. "Pi").
func FindConstantByName(ast map[string]any, name string) map[string]any {
	mod := FindModule(ast)
	if mod == nil {
		return nil
	}
	decls, ok := mod["declarations"].([]any)
	if !ok {
		return nil
	}
	for _, d := range decls {
		decl, _ := d.(map[string]any)
		if decl["type"] != "ConstantDecl" {
			continue
		}
		ident, _ := decl["name"].(map[string]any)
		if ident["name"] == name {
			return decl
		}
	}
	return nil
}

// FindTypeByName returns a TypeDecl by name (e.g. "Point").
func FindTypeByName(ast map[string]any, name string) map[string]any {
	mod := FindModule(ast)
	if mod == nil {
		return nil
	}
	decls, ok := mod["declarations"].([]any)
	if !ok {
		return nil
	}
	for _, d := range decls {
		decl, _ := d.(map[string]any)
		if decl["type"] != "TypeDecl" {
			continue
		}
		ident, _ := decl["name"].(map[string]any)
		if ident["name"] == name {
			return decl
		}
	}
	return nil
}

// FindImportByAlias returns an Import node by alias (e.g. "Out").
func FindImportByAlias(ast map[string]any, alias string) map[string]any {
	mod := FindModule(ast)
	if mod == nil {
		return nil
	}
	decls, ok := mod["declarations"].([]any)
	if !ok {
		return nil
	}
	for _, d := range decls {
		decl, _ := d.(map[string]any)
		if decl["type"] != "Import" {
			continue
		}
		if decl["alias"] == alias {
			return decl
		}
	}
	return nil
}
