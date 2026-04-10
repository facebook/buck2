/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package gobuckifylib

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"os"
	"os/exec"
	"slices"
	"strings"
	"sync"
)

// Module represents a Go module from go list output
type Module struct {
	Path    string `json:"Path"`
	Version string `json:"Version"`
}

// Package is a subset of the fields of the golist output
type Package struct {
	Name       string
	ImportPath string
	Imports    []string
	EmbedFiles []string
	Standard   bool
	Module     *Module
}

func QueryGoList(workDir, rootModuleName string, extraArgs ...string) (chan *Package, chan error) {
	pkgChan := make(chan *Package, 1000) // 1000 is a guess, but should be enough
	errChan := make(chan error, 1)
	go func() {
		defer close(pkgChan)
		defer close(errChan)
		cmdArgs := slices.Concat(
			[]string{"buck2", "run", "toolchains//:go[go]", "--", "list", "-C", workDir, "-e", "-json=Name,ImportPath,Imports,EmbedFiles,Standard,Module"},
			extraArgs,
			[]string{"all"},
		)
		cmd := exec.Command(cmdArgs[0], cmdArgs[1:]...)
		// need for consistent behaviour on any host machine
		cmd.Env = append(os.Environ(), "CGO_ENABLED=1")

		stdout, err := cmd.StdoutPipe()
		if err != nil {
			errChan <- fmt.Errorf("failed to get stdout pipe: %w", err)
			return
		}

		stderr, err := cmd.StderrPipe()
		if err != nil {
			errChan <- fmt.Errorf("failed to get stderr pipe: %w", err)
			return
		}

		if err := cmd.Start(); err != nil {
			errChan <- fmt.Errorf("failed to start go list: %w", err)
			return
		}

		for dec := json.NewDecoder(stdout); dec.More(); {
			var pkg Package
			if err := dec.Decode(&pkg); err != nil {
				errChan <- fmt.Errorf("failed to decode json: %w", err)
				return
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
			pkgChan <- &pkg
		}

		stderrBytes, err := io.ReadAll(stderr)
		if err != nil {
			errChan <- fmt.Errorf("failed to read stderr: %w", err)
			return
		}

		if err := cmd.Wait(); err != nil {
			fmt.Fprintln(os.Stderr, string(stderrBytes))
			errChan <- fmt.Errorf("failed to wait for go list: %w", err)
		}
	}()

	return pkgChan, errChan
}

func ReadModuleName(path string) (string, error) {
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

// CollectPackagesResult contains the results of collecting packages from go list
type CollectPackagesResult struct {
	BuckTargets BuckTargets
	Modules     map[string]*Module
}

// CollectPackages queries go list for all platforms and collects packages into BuckTargets and unique Modules
func CollectPackages(cfg *Config, thirdPartyDir, rootModuleName string) (*CollectPackagesResult, error) {
	type result struct {
		pkg      *Package
		buckOS   string
		buckArch string
	}

	results := make(chan *result, 1000*len(cfg.Platforms))
	mainErrChan := make(chan error)

	// Limit concurrency to avoid OOMs as `go list` can use a lot of memory
	maxConcurrency := 10
	semaphore := make(chan struct{}, maxConcurrency)

	wg := sync.WaitGroup{}
	for _, p := range cfg.Platforms {
		wg.Add(1)
		go func() {
			defer wg.Done()

			semaphore <- struct{}{}
			defer func() { <-semaphore }()

			tags := slices.Concat([]string{p.GoOS, p.GoArch}, cfg.DefaultTags)
			pkgChan, errChan := QueryGoList(thirdPartyDir, rootModuleName, fmt.Sprintf("-tags=%s", strings.Join(tags, ",")))
			pkgCount := 0
			for pkg := range pkgChan {
				pkgCount++
				results <- &result{pkg: pkg, buckOS: p.BuckOS, buckArch: p.BuckArch}
			}
			slog.Info("Found packages", "count", pkgCount, "os", p.GoOS, "arch", p.GoArch)
			for err := range errChan {
				mainErrChan <- fmt.Errorf("error querying golist for %s: %w", p, err)
			}
		}()
	}

	go func() {
		wg.Wait()
		close(results)
		close(mainErrChan)
	}()

	buckTargets := make(BuckTargets)
	modules := make(map[string]*Module)

	resultsClosed, mainErrChanClosed, hadErrors := false, false, false
	for {
		if resultsClosed && mainErrChanClosed {
			break
		}
		select {
		case res, ok := <-results:
			if !ok {
				resultsClosed = true
				continue
			}
			buckTargets.AddPackage(res.pkg, res.buckOS, res.buckArch)
			if res.pkg.Module != nil {
				modules[res.pkg.Module.Path] = res.pkg.Module
			}
		case err, ok := <-mainErrChan:
			if !ok {
				mainErrChanClosed = true
				continue
			}
			slog.Error("Error querying golist", "err", err)
			hadErrors = true
		}
	}

	if hadErrors {
		return nil, fmt.Errorf("errors occurred while collecting packages")
	}

	slog.Info("Packages collected", "count", len(buckTargets))
	slog.Info("Modules collected", "count", len(modules))

	return &CollectPackagesResult{
		BuckTargets: buckTargets,
		Modules:     modules,
	}, nil
}
