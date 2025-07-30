/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"slices"
	"strings"
)

// Package is a subset of the fields of the golist output
type Package struct {
	Name       string
	ImportPath string
	Imports    []string
	EmbedFiles []string
	Standard   bool
	Module     interface{}
}

func queryGoList(workDir, rootModuleName string, extraArgs ...string) (map[string]*Package, error) {
	cmdArgs := slices.Concat(
		[]string{"buck2", "run", "toolchains//:go[go]", "--", "list", "-C", workDir, "-e", "-json=Name,ImportPath,Imports,EmbedFiles,Standard,Module"},
		extraArgs,
		[]string{"all"},
	)
	cmd := exec.Command(cmdArgs[0], cmdArgs[1:]...)
	// need for consistent behaviour on any host machine
	cmd.Env = append(os.Environ(), "CGO_ENABLED=1")
	out, err := cmd.Output()
	if err != nil {
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) {
			fmt.Println(string(exitErr.Stderr))
		}
		return nil, err
	}

	pkgs := make(map[string]*Package)
	for dec := json.NewDecoder(bytes.NewBuffer(out)); dec.More(); {
		var pkg Package
		if err := dec.Decode(&pkg); err != nil {
			return nil, err
		}
		if pkg.Standard {
			continue // skip standard library packages
		}
		if pkg.Module == nil {
			continue // that's not a ligit third-party package
		}
		if strings.HasPrefix(pkg.ImportPath, rootModuleName) {
			continue // skip the root module packages
		}
		pkgs[pkg.ImportPath] = &pkg
	}

	return pkgs, nil
}

func readModuleName(path string) (string, error) {
	f, err := os.Open(path)
	if err != nil {
		return "", fmt.Errorf("failed to open %s: %w", path, err)
	}
	defer f.Close()

	reader := bufio.NewReader(f)
	line, err := reader.ReadString('\n')
	if err != nil && !errors.Is(err, io.EOF) {
		return "", fmt.Errorf("failed to read first line: %w", err)
	}
	parts := strings.SplitN(strings.TrimSpace(line), " ", 2)
	if len(parts) != 2 {
		return "", fmt.Errorf("failed to parse first line: %s", line)
	}
	return parts[1], nil
}
