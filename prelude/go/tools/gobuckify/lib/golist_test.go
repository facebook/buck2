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
	"os"
	"path/filepath"
	"testing"
)

// Only the root module's own packages are skipped.
func TestOnlyPackagesOfTheRootModuleAreSkipped(t *testing.T) {
	if isRootModulePackage("example.com/rootkit", "example.com/root") {
		t.Fatal("example.com/rootkit was skipped")
	}
	for _, p := range []string{"example.com/root", "example.com/root/sub"} {
		if !isRootModulePackage(p, "example.com/root") {
			t.Fatalf("%s was not skipped", p)
		}
	}
}

func writeGoMod(t *testing.T, content string) string {
	t.Helper()
	p := filepath.Join(t.TempDir(), "go.mod")
	if err := os.WriteFile(p, []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
	return p
}

// The module name is the second word of the first line, whatever that line is.
func TestModuleNameIsTakenFromTheFirstLineOnly(t *testing.T) {
	cases := []struct{ content, want string }{
		{"// the root module\nmodule example.com/root\n", "the root module"},
		{"module \"example.com/root\"\n", "\"example.com/root\""},
		{"module example.com/root // the root module\n", "example.com/root // the root module"},
	}
	for _, c := range cases {
		got, err := ReadModuleName(writeGoMod(t, c.content))
		if err != nil {
			t.Fatalf("%q: %v", c.content, err)
		}
		if got != c.want {
			t.Fatalf("%q: got %q, want %q", c.content, got, c.want)
		}
	}
	if _, err := ReadModuleName(writeGoMod(t, "\nmodule example.com/root\n")); err == nil {
		t.Fatal("a leading blank line was accepted")
	}
}
