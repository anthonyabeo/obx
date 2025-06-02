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
	lines := strings.Split(string(data), "\n")

	var headers []Header
	var current *Header

	for _, rawLine := range lines {
		line := strings.TrimSpace(rawLine)

		if strings.HasPrefix(line, "MODULE ") {
			if current != nil {
				headers = append(headers, *current)
			}
			modName := strings.TrimSuffix(strings.TrimSpace(strings.TrimPrefix(line, "MODULE ")), ";")
			current = &Header{
				ID:   ModuleID{Name: modName},
				File: path,
			}
		} else if current != nil {
			if strings.HasPrefix(line, "IMPORT ") {
				importList := strings.TrimSuffix(strings.TrimSpace(strings.TrimPrefix(line, "IMPORT ")), ";")
				parts := strings.Split(importList, ",")
				for _, part := range parts {
					part = strings.TrimSpace(part)
					spec := parseImportSpec(part)
					current.Imports = append(current.Imports, spec)
				}
			}
			if strings.HasPrefix(line, "END ") {
				headers = append(headers, *current)
				current = nil
			}
		}
	}

	if current != nil {
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

	modID := ModuleID{}
	if strings.Contains(module, ".") {
		segs := strings.SplitN(module, ".", 2)
		modID = ModuleID{
			Path: strings.TrimSpace(segs[0]),
			Name: strings.TrimSpace(segs[1]),
		}
	} else {
		modID = ModuleID{Name: module}
	}

	return Import{Alias: alias, ID: modID}
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
