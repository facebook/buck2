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
	"reflect"
	"strings"
	"testing"
)

// A package directory whose embedded tree holds a nested module and a VCS directory.
func embedFixture(t *testing.T) string {
	t.Helper()
	pkgdir := t.TempDir()
	for _, f := range []string{"static/a.txt", "static/mod/go.mod", "static/mod/m.txt", "static/.git/config"} {
		p := filepath.Join(pkgdir, filepath.FromSlash(f))
		if err := os.MkdirAll(filepath.Dir(p), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(p, []byte(f), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return pkgdir
}

// Files under a nested module are embedded, and naming a VCS directory embeds
// its contents, both unlike `go build`.
func TestNestedModuleAndNamedVcsDirectoryAreEmbedded(t *testing.T) {
	pkgdir := embedFixture(t)
	files, _, err := resolveEmbed(pkgdir, []string{"static"})
	if err != nil {
		t.Fatal(err)
	}
	want := []string{"static/a.txt", "static/mod/go.mod", "static/mod/m.txt"}
	if !reflect.DeepEqual(files, want) {
		t.Fatalf("static: got %v, want %v", files, want)
	}
	files, _, err = resolveEmbed(pkgdir, []string{"static/.git"})
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(files, []string{"static/.git/config"}) {
		t.Fatalf("static/.git: got %v", files)
	}
	if !strings.HasSuffix(files[0], "config") {
		t.Fatalf("unexpected file %q", files[0])
	}
}
