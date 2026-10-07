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
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// generateMain runs the generator on a one-test package with the given flags and
// returns the generated main.go.
func generateMain(t *testing.T, flags ...string) string {
	t.Helper()
	dir := t.TempDir()
	testFile := filepath.Join(dir, "x_test.go")
	src := "package x\n\nimport \"testing\"\n\nfunc TestA(t *testing.T) {}\n"
	if err := os.WriteFile(testFile, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	// The flag values live in package globals, so clear what an earlier run left.
	for k := range coverPkgs {
		delete(coverPkgs, k)
	}
	out := filepath.Join(dir, "main.go")
	os.Args = append([]string{"testmaingen", "--output", out}, flags...)
	os.Args = append(os.Args, testFile)
	main()
	data, err := os.ReadFile(out)
	if err != nil {
		t.Fatal(err)
	}
	return string(data)
}

func coveredLine(mainGo string) string {
	for _, line := range strings.Split(mainGo, "\n") {
		if strings.Contains(line, "testdeps.Covered = ") {
			return strings.TrimSpace(line)
		}
	}
	return ""
}

// The cover packages come out in Go map iteration order, so the same inputs
// give different generated files from one run to the next.
func TestCoverPackagesComeOutInMapOrder(t *testing.T) {
	seen := map[string]bool{}
	for i := 0; i < 40; i++ {
		seen[coveredLine(generateMain(t, "--cover-mode=set", "--cover-pkgs=foo/a,foo/b,foo/c"))] = true
	}
	if len(seen) < 2 {
		t.Fatalf("expected more than one order over 40 runs, got %v", seen)
	}
	for line := range seen {
		if !strings.HasPrefix(line, `testdeps.Covered = " in foo/`) {
			t.Fatalf("unexpected line %q", line)
		}
	}
}

// With a cover mode but no cover packages the message is a dangling " in ".
func TestNoCoverPackagesGivesDanglingIn(t *testing.T) {
	if got := coveredLine(generateMain(t, "--cover-mode=set")); got != `testdeps.Covered = " in "` {
		t.Fatalf("got %q", got)
	}
}
