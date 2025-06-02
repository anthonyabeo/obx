package modgraph

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

func ScanModuleHeaders(path string) ([]Header, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("unable to read file: %w", err)
	}

	var headers []Header
	var current *Header

	offset := 0 // track current byte offset in file
	lines := strings.Split(string(data), "\n")

	for _, rawLine := range lines {
		line := strings.TrimSpace(rawLine)
		lineLen := len(rawLine) + 1 // +1 for '\n', assumes Unix line endings

		if strings.HasPrefix(line, "MODULE ") {
			if current != nil {
				// close previous module: set end offset to start of this line - 1
				current.EndPos = offset - 1
				headers = append(headers, *current)
			}
			modName := strings.TrimSuffix(strings.TrimSpace(strings.TrimPrefix(line, "MODULE ")), ";")
			current = &Header{
				ID:       nextModID(),
				Name:     modName,
				File:     path,
				StartPos: offset,
			}
		} else if current != nil {
			if strings.HasPrefix(line, "IMPORT ") {
				importList := strings.TrimSuffix(strings.TrimSpace(strings.TrimPrefix(line, "IMPORT ")), ";")
				parts := strings.Split(importList, ",")
				for _, part := range parts {
					part = strings.TrimSpace(part)
					spec := parseImportSpec(part) // your existing alias/path parser
					current.Imports = append(current.Imports, spec)
				}
			}
			if strings.HasPrefix(line, "END ") && strings.Contains(line, current.Name) {
				// mark module end after this line
				current.EndPos = offset + lineLen - 1
				headers = append(headers, *current)
				current = nil
			}
		}

		offset += lineLen
	}

	// If file ended without END
	if current != nil {
		current.EndPos = len(data)
		headers = append(headers, *current)
	}

	if len(headers) == 0 {
		return nil, fmt.Errorf("no modules found in file: %s", path)
	}
	return headers, nil
}

func parseImportSpec(spec string) Import {
	spec = strings.TrimSpace(spec)
	var alias string
	var module string

	if strings.Contains(spec, ":=") {
		parts := strings.SplitN(spec, ":=", 2)
		alias = strings.TrimSpace(parts[0])
		module = strings.TrimSpace(parts[1])
	} else {
		module = spec
	}

	var path, name string
	if strings.Contains(module, ".") {
		segs := strings.SplitN(module, ".", 2)
		path = strings.TrimSpace(segs[0])
		name = strings.TrimSpace(segs[1])
	} else {
		name = module
	}

	return Import{Alias: alias, Name: name, Path: path}
}

func DiscoverModuleFiles(root string) ([]string, error) {
	var files []string
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if filepath.Ext(path) == ".obx" {
			files = append(files, path)
		}
		return nil
	})
	return files, err
}
