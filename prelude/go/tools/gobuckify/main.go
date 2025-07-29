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
	"slices"
	"strings"
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

	buckTargets := make(BuckTargets)
	for _, p := range cfg.Platforms {
		tags := slices.Concat([]string{p.GoOS, p.GoArch}, cfg.DefaultTags)
		pkgs, err := queryGoList(thirdPartyDir, rootModuleName, fmt.Sprintf("-tags=%s", strings.Join(tags, ",")))
		if err != nil {
			slog.Error("Error querying golist", "err", err)
			os.Exit(1)
		}
		slog.Info("Found packages", "count", len(pkgs), "os", p.GoOS, "arch", p.GoArch)
		for _, pkg := range pkgs {
			buckTargets.AddPackage(pkg, p.BuckOS, p.BuckArch)
		}
	}

	for _, target := range buckTargets {
		target.Normalise(len(cfg.Platforms))
	}
	slog.Info("Packages collected", "count", len(buckTargets))
	slog.Info("Rendering BUCK files")
	if err := renderBuckFiles(cfg, thirdPartyDir, buckTargets); err != nil {
		slog.Error("Error rendering BUCK files", "err", err)
		os.Exit(1)
	}
}
