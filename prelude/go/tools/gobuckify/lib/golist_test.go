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

// `example.com/rootkit` is another module, but its path starts with the root
// module's path, so it is skipped and gets no BUCK file.
func TestModuleSharingTheRootPathPrefixIsSkipped(t *testing.T) {
	if !isRootModulePackage("example.com/rootkit", "example.com/root") {
		t.Fatal("example.com/rootkit was not skipped")
	}
	if !isRootModulePackage("example.com/root/sub", "example.com/root") {
		t.Fatal("example.com/root/sub was not skipped")
	}
}
