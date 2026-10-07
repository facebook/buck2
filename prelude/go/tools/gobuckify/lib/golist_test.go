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
