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
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// getGoBinary creates a temporary Go binary wrapper that uses Buck2
// Won't work on Windows
func getGoBinary() (string, func(), error) {
	tmpDir, err := os.MkdirTemp("", "go-bin-*")
	if err != nil {
		return "", func() {}, fmt.Errorf("failed to create temp dir: %w", err)
	}

	goBinaryPath := filepath.Join(tmpDir, "go")
	scriptContent := "#!/usr/bin/env bash\nexec buck2 run $GOPACKAGESDRIVER_BUCK_COMMON_FLAGS 'toolchains//:go[go]' -- \"$@\"\n"

	err = os.WriteFile(goBinaryPath, []byte(scriptContent), 0755)
	if err != nil {
		return "", func() {}, fmt.Errorf("failed to write to temp file: %w", err)
	}

	cleanup := func() {
		_ = os.RemoveAll(tmpDir)
	}

	return goBinaryPath, cleanup, nil
}

func getGoRoot(goBinary string) (string, error) {
	out, err := exec.Command(goBinary, "env", "GOROOT").Output()
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(out)), nil
}
