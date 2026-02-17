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
	"fmt"
	"log/slog"
	"os"
	"path/filepath"
	"strings"
	"sync"
)

func main() {
	if len(os.Args[1:]) != 1 {
		fmt.Println("Usage: gobuckify <path-third-party-dir>")
		os.Exit(1)
	}

	thirdPartyDir := os.Args[1]

	goMod := filepath.Join(thirdPartyDir, "go.mod")
	rootModuleName, err := readModuleName(goMod)
	if err != nil {
		slog.Error("Error reading go.mod", "err", err)
		os.Exit(1)
	}

	cfg, err := FromJSON(filepath.Join(thirdPartyDir, "gobuckify.json"))
	if err != nil {
		slog.Error("Error reading config gobuckify.json", "err", err)
		os.Exit(1)
	}

	type result struct {
		pkg      *Package
		buckOS   string
		buckArch string
	}

	results := make(chan *result, 1000*len(cfg.Platforms)) // should be enough
	mainErrChan := make(chan error)

	// Limit concurrency to avoid OOMs as `go list` can use a lot of memory
	// It used 300MB for 7k packages on my tests, so 10 should be reasonable number
	maxConcurrency := 10
	semaphore := make(chan struct{}, maxConcurrency)

	wg := sync.WaitGroup{}
	for _, p := range cfg.Platforms {
		wg.Add(1)
		go func() {
			defer wg.Done()

			// Acquire semaphore
			semaphore <- struct{}{}
			defer func() { <-semaphore }()

			var extraArgs []string
			if len(cfg.DefaultTags) > 0 {
				extraArgs = []string{fmt.Sprintf("-tags=%s", strings.Join(cfg.DefaultTags, ","))}
			}
			pkgChan, errChan := queryGoList(thirdPartyDir, rootModuleName, p.GoOS, p.GoArch, extraArgs...)
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
		os.Exit(1)
	}
	slog.Info("Packages collected", "count", len(buckTargets))
	slog.Info("Rendering BUCK files")
	if err := renderBuckFiles(cfg, thirdPartyDir, buckTargets); err != nil {
		slog.Error("Error rendering BUCK files", "err", err)
		os.Exit(1)
	}
}
