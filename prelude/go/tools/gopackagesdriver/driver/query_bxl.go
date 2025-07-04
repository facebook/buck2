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
	"context"
	"encoding/json"
	"errors"
	"log/slog"
	"os"
	"os/exec"
	"strings"

	"golang.org/x/tools/go/packages"
)

// LoadModeTypecheckCgo is a hack to support cgo in gopls
// We declare a copy of private value that for some reason is used in gopls
// Ideally we should not do this, but it's the only way to support cgo in gopls
const LoadModeTypecheckCgo = packages.LoadMode(4096)

const cgoGoTypesFileName = "_cgo_gotypes.go"
const cgoGenFileNameExt = ".cgo1.go"

// queryBXL is a wrapper around query that will use BXL to resolve the targets
func queryBXL(ctx context.Context, req *packages.DriverRequest, bucker Bucker, platform Platform, patterns []string, files []string) (*packages.DriverResponse, error) {
	if len(patterns) == 0 && len(files) == 0 {
		return &packages.DriverResponse{}, nil
	}

	bxlArgs := buildBXLArgs(req, patterns, files)

	bxlOut, err := bucker.BXL(ctx, "prelude//go/tools/gopackagesdriver:driver.bxl:driver", bxlArgs)
	if err != nil {
		var ee *exec.ExitError
		if errors.As(err, &ee) {
			// Create ad-hoc package with information about the error
			// This is likely will be done inside BXL
			// once  https://fburl.com/workplace/q79a59rn implemented
			actionErrors := retriveActionErrors(ee.Stderr)
			if len(actionErrors) > 0 {
				slog.Warn("failed to query BXL, but we have action errors", "err", err)
				return &packages.DriverResponse{
					Roots: []string{"ad-hoc-error-recovery"},
					Packages: []*packages.Package{{
						ID:              "ad-hoc-error-recovery",
						GoFiles:         files,
						CompiledGoFiles: files,
						Errors: []packages.Error{{
							Msg:  actionErrors,
							Kind: packages.ListError,
						}},
					}},
				}, nil
			}
		}

		return nil, err
	}

	filePath := strings.TrimSuffix(string(bxlOut), "\n")

	file, err := os.ReadFile(filePath)
	if err != nil {
		return nil, err
	}
	slog.Debug("Response from query BXL", "file", file)

	var response packages.DriverResponse
	err = json.Unmarshal(file, &response)
	if err != nil {
		return nil, err
	}

	// Fix CGO paths in compiled Go files
	for _, pkg := range response.Packages {
		for _, file := range pkg.CompiledGoFiles {
			if strings.HasSuffix(file, cgoGoTypesFileName) || strings.HasSuffix(file, cgoGenFileNameExt) {
				_ = fixRePath(platform, file)
			}
		}
	}

	return &response, nil
}

func buildBXLArgs(req *packages.DriverRequest, patterns []string, files []string) []string {
	bxlArgs := []string{}
	if len(patterns) > 0 {
		bxlArgs = append(bxlArgs, "--target_exprs")
		bxlArgs = append(bxlArgs, patterns...)
	}
	if len(files) > 0 {
		bxlArgs = append(bxlArgs, "--files")
		bxlArgs = append(bxlArgs, files...)
	}
	if req.Tests {
		bxlArgs = append(bxlArgs, "--need_tests", "true")
	}
	if req.Mode&packages.NeedName != 0 {
		bxlArgs = append(bxlArgs, "--need_name", "true")
	}
	// we have to implicitely add some flags if syntax/types requested
	// same as `go list` driver does
	// todo: add packages.NeedTypesSizes to the query as well, when we fix slow builds
	// as gopls requires it, but it can't wait very long
	needTypes := packages.NeedTypes | packages.NeedTypesInfo
	if req.Mode&(packages.NeedFiles|packages.NeedSyntax|needTypes) != 0 {
		bxlArgs = append(bxlArgs, "--need_files", "true")
	}
	if req.Mode&(packages.NeedCompiledGoFiles|packages.NeedSyntax|needTypes) != 0 {
		bxlArgs = append(bxlArgs, "--need_compiled_go_files", "true")
	}
	if req.Mode&(packages.NeedImports|needTypes) != 0 {
		bxlArgs = append(bxlArgs, "--need_imports", "true")
	}
	if req.Mode&(packages.NeedDeps|needTypes) != 0 {
		bxlArgs = append(bxlArgs, "--need_deps", "true")
	}
	if req.Mode&(packages.NeedExportFile|needTypes) != 0 {
		bxlArgs = append(bxlArgs, "--need_export_file", "true")
	}
	if req.Mode&LoadModeTypecheckCgo != 0 {
		bxlArgs = append(bxlArgs, "--typecheck_cgo", "true")
	}
	return bxlArgs
}
