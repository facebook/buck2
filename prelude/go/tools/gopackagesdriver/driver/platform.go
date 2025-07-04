/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package driver

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"golang.org/x/tools/go/packages"
)

// Platform is an interface to any system level operation we need
// for example opening files or finding unique file paths
type Platform interface {
	ProjectDir() string
	// Returns true if the file exists on the filesystem
	FileExists(path string) bool
}

type realPlatform struct {
	// absolute goroot path
	projectDir string
	overlay    map[string][]byte
}

func (p *realPlatform) ProjectDir() string {
	return p.projectDir
}

// OverlayOpener creates a closure over an FS overlay that allows for overrides of FS reads
func OverlayOpener(overlay map[string][]byte) func(path string) (io.ReadCloser, error) {
	return func(path string) (io.ReadCloser, error) {
		ol := overlay
		if data, ok := ol[path]; ok {
			return io.NopCloser(bytes.NewReader(data)), nil
		}
		return os.Open(path)
	}
}

func (p *realPlatform) FileExists(path string) bool {
	if _, ok := p.overlay[path]; ok {
		return true
	}
	info, err := os.Stat(path)
	if os.IsNotExist(err) {
		return false
	}
	return !info.IsDir()
}

// CWD returns the current WD with symlinks resolved
func CWD() string {
	cwd, _ := os.Getwd()
	cwd, _ = filepath.EvalSymlinks(cwd)
	return cwd
}

// findProjectDirectory finds the absolute buck project directory
func findProjectDirectory(ctx context.Context, bucker Bucker) (string, error) {
	return bucker.Root(ctx)
}

// newPlatform creates a new actual platform using system libraries and buck
func newPlatform(ctx context.Context, bucker Bucker, req *packages.DriverRequest) (*realPlatform, error) {
	rp := &realPlatform{
		overlay: req.Overlay,
	}

	pd, err := findProjectDirectory(ctx, bucker)
	if err != nil || pd == "" {
		return nil, fmt.Errorf("failed to find buck project directory: %w", err)
	}
	rp.projectDir = pd

	return rp, nil
}
