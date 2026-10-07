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
	"fmt"
	"strings"
	"testing"

	"golang.org/x/tools/go/packages"
)

// A package without imports, as the BXL response describes it, whose only file
// gained `import "fmt"` in the editor.
func overlayImportFixture() (*packages.Package, map[string][]byte, map[string]*packages.Package, map[string]bool) {
	pkg := &packages.Package{ID: "root//a:a", PkgPath: "a", GoFiles: []string{"/repo/a/a.go"}}
	overlay := map[string][]byte{"/repo/a/a.go": []byte("package a\n\nimport \"fmt\"\n\nvar _ = fmt.Sprint\n")}
	std := map[string]*packages.Package{"fmt": {ID: "fmt", PkgPath: "fmt"}}
	return pkg, overlay, std, map[string]bool{}
}

// The overlay path writes into the package's nil Imports map, so editing a
// package that has no imports yet crashes the driver.
func TestOverlayImportIntoPackageWithoutImportsPanics(t *testing.T) {
	pkg, overlay, std, imported := overlayImportFixture()
	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("expected a panic")
		}
		if !strings.Contains(fmt.Sprint(r), "assignment to entry in nil map") {
			t.Fatalf("unexpected panic: %v", r)
		}
	}()
	addOverlayImports(pkg, overlay, std, imported)
}
