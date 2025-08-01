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
	"reflect"
	"slices"
	"testing"
)

func TestTargetNameFromImportPath(t *testing.T) {
	tests := []struct {
		name       string
		importPath string
		want       string
	}{
		{
			name:       "simple path",
			importPath: "github.com/example/pkg",
			want:       "pkg",
		},
		{
			name:       "no slashes",
			importPath: "my-lib.com",
			want:       "my-lib.com",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := targetNameFromImportPath(tt.importPath)
			if got != tt.want {
				t.Errorf("targetNameFromImportPath(%q) = %q, want %q", tt.importPath, got, tt.want)
			}
		})
	}
}

func TestTargetLabelFromImportPath(t *testing.T) {
	want := "//prefix/github.com/example/pkg:pkg"
	got := targetLabelFromImportPath("//prefix/", "github.com/example/pkg")
	if got != want {
		t.Errorf("targetLabelFromImportPath(%q, %q) = %q, want %q",
			"//prefix/", "github.com/example/pkg", got, want)
	}
}

func TestBuckTargetNormalise(t *testing.T) {
	tests := []struct {
		name                     string
		target                   *BuckTarget
		totalPlatformNumber      int
		wantCommonDeps           []string
		wantPlatformDepsLen      int
		wantTargetCompatibleWith map[string][]string
	}{
		{
			name: "move common deps and clean-up target_compatible_with",
			target: &BuckTarget{
				Name:       "test",
				ImportPath: "github.com/example/test",
				CommonDeps: []string{},
				PlatformDeps: map[string]*OSDeps{
					"linux": {
						OS: "linux",
						ArchDeps: map[string]*ArchDeps{
							"x86_64": {
								Arch: "x86_64",
								Deps: func() *stringSet {
									s := newSet()
									s.Add("common/dep1")
									s.Add("common/dep2")
									s.Add("linux/dep")
									return s
								}(),
							},
						},
					},
					"darwin": {
						OS: "darwin",
						ArchDeps: map[string]*ArchDeps{
							"x86_64": {
								Arch: "x86_64",
								Deps: func() *stringSet {
									s := newSet()
									s.Add("common/dep1")
									s.Add("common/dep2")
									s.Add("darwin/dep")
									return s
								}(),
							},
						},
					},
				},
				TargetCompatibleWith: map[string][]string{
					"linux":  {"x86_64"},
					"darwin": {"x86_64"},
				},
			},
			totalPlatformNumber:      2,
			wantCommonDeps:           []string{"common/dep1", "common/dep2"},
			wantPlatformDepsLen:      2, // Both platforms still have platform-specific deps
			wantTargetCompatibleWith: nil,
		},
		{
			name: "remove empty platform deps",
			target: &BuckTarget{
				Name:       "test",
				ImportPath: "github.com/example/test",
				CommonDeps: []string{},
				PlatformDeps: map[string]*OSDeps{
					"linux": {
						OS: "linux",
						ArchDeps: map[string]*ArchDeps{
							"x86_64": {
								Arch: "x86_64",
								Deps: newSet(),
							},
						},
					},
					"darwin": {
						OS: "darwin",
						ArchDeps: map[string]*ArchDeps{
							"x86_64": {
								Arch: "x86_64",
								Deps: newSet(),
							},
						},
					},
				},
				TargetCompatibleWith: map[string][]string{
					"linux":  {"x86_64"},
					"darwin": {"x86_64"},
				},
			},
			totalPlatformNumber: 4,
			wantCommonDeps:      []string{},
			wantPlatformDepsLen: 0, // All platform deps should be removed as they're now empty
			wantTargetCompatibleWith: map[string][]string{
				"linux":  {"x86_64"},
				"darwin": {"x86_64"},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tt.target.Normalise(tt.totalPlatformNumber)

			// Check CommonDeps
			if !reflect.DeepEqual(tt.target.CommonDeps, tt.wantCommonDeps) {
				t.Errorf("CommonDeps = %v, want %v", tt.target.CommonDeps, tt.wantCommonDeps)
			}

			// Check PlatformDeps length
			if len(tt.target.PlatformDeps) != tt.wantPlatformDepsLen {
				t.Errorf("PlatformDeps length = %d, want %d", len(tt.target.PlatformDeps), tt.wantPlatformDepsLen)
			}

			// Check TargetCompatibleWith
			if !reflect.DeepEqual(tt.target.TargetCompatibleWith, tt.wantTargetCompatibleWith) {
				t.Errorf("TargetCompatibleWith = %v, want %v", tt.target.TargetCompatibleWith, tt.wantTargetCompatibleWith)
			}
		})
	}
}

func TestBuckTargetsAddPackage(t *testing.T) {
	tests := []struct {
		name           string
		pkg            *Package
		buckOS         string
		buckArch       string
		wantName       string
		wantImportPath string
		wantIsBinary   bool
		wantEmbedFiles []string
		wantDepsCount  int
	}{
		{
			name: "add library package",
			pkg: &Package{
				Name:       "testpkg",
				ImportPath: "github.com/example/testpkg",
				Imports:    []string{"github.com/example/dep1", "github.com/example/dep2", "fmt"},
				EmbedFiles: []string{"embed1.txt", "embed2.txt"},
			},
			buckOS:         "linux",
			buckArch:       "x86_64",
			wantName:       "testpkg",
			wantImportPath: "github.com/example/testpkg",
			wantIsBinary:   false,
			wantEmbedFiles: []string{"embed1.txt", "embed2.txt"},
			wantDepsCount:  2, // Only non-stdlib deps
		},
		{
			name: "add binary package",
			pkg: &Package{
				Name:       "main",
				ImportPath: "github.com/example/cmd",
				Imports:    []string{"github.com/example/dep1", "fmt"},
				EmbedFiles: []string{},
			},
			buckOS:         "darwin",
			buckArch:       "arm64",
			wantName:       "cmd",
			wantImportPath: "github.com/example/cmd",
			wantIsBinary:   true,
			wantEmbedFiles: []string{},
			wantDepsCount:  1, // Only non-stdlib deps
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			targets := make(BuckTargets)
			targets.AddPackage(tt.pkg, tt.buckOS, tt.buckArch)

			// Check if the package was added
			target, exists := targets[tt.pkg.ImportPath]
			if !exists {
				t.Fatalf("Package not added to targets")
			}

			// Check basic properties
			if target.Name != tt.wantName {
				t.Errorf("Target name = %q, want %q", target.Name, tt.wantName)
			}

			if target.ImportPath != tt.wantImportPath {
				t.Errorf("Target import path = %q, want %q", target.ImportPath, tt.wantImportPath)
			}

			if target.IsBinary != tt.wantIsBinary {
				t.Errorf("Target IsBinary = %v, want %v", target.IsBinary, tt.wantIsBinary)
			}

			if !reflect.DeepEqual(target.EmbedFiles.SortedList(), tt.wantEmbedFiles) {
				t.Errorf("Target EmbedFiles = %v, want %v", target.EmbedFiles.SortedList(), tt.wantEmbedFiles)
			}

			// Check platform deps
			osDeps, exists := target.PlatformDeps[tt.buckOS]
			if !exists {
				t.Fatalf("OS deps not found for %s", tt.buckOS)
			}

			archDeps, exists := osDeps.ArchDeps[tt.buckArch]
			if !exists {
				t.Fatalf("Arch deps not found for %s", tt.buckArch)
			}

			if archDeps.Deps.Len() != tt.wantDepsCount {
				t.Errorf("Deps count = %d, want %d", archDeps.Deps.Len(), tt.wantDepsCount)
			}

			// Check TargetCompatibleWith
			archList, exists := target.TargetCompatibleWith[tt.buckOS]
			if !exists {
				t.Fatalf("TargetCompatibleWith not found for %s", tt.buckOS)
			}

			if !slices.Contains(archList, tt.buckArch) {
				t.Errorf("Arch %s not found in TargetCompatibleWith for OS %s", tt.buckArch, tt.buckOS)
			}
		})
	}
}

func TestAddPackageMultiplePlatforms(t *testing.T) {
	targets := make(BuckTargets)
	pkg := &Package{
		Name:       "testpkg",
		ImportPath: "github.com/example/testpkg",
		Imports:    []string{"github.com/example/dep1", "github.com/example/dep2"},
	}

	// Add the same package for multiple platforms
	targets.AddPackage(pkg, "linux", "x86_64")
	targets.AddPackage(pkg, "linux", "arm64")
	targets.AddPackage(pkg, "darwin", "x86_64")

	target, exists := targets[pkg.ImportPath]
	if !exists {
		t.Fatalf("Package not added to targets")
	}

	// Check that all platforms were added
	if len(target.TargetCompatibleWith) != 2 {
		t.Errorf("Expected 2 OS entries in TargetCompatibleWith, got %d", len(target.TargetCompatibleWith))
	}

	// Check linux architectures
	linuxArchs, exists := target.TargetCompatibleWith["linux"]
	if !exists {
		t.Fatalf("Linux not found in TargetCompatibleWith")
	}
	if len(linuxArchs) != 2 {
		t.Errorf("Expected 2 architectures for Linux, got %d", len(linuxArchs))
	}

	// Check darwin architectures
	darwinArchs, exists := target.TargetCompatibleWith["darwin"]
	if !exists {
		t.Fatalf("Darwin not found in TargetCompatibleWith")
	}
	if len(darwinArchs) != 1 {
		t.Errorf("Expected 1 architecture for Darwin, got %d", len(darwinArchs))
	}

	// Check platform deps
	if len(target.PlatformDeps) != 2 {
		t.Errorf("Expected 2 OS entries in PlatformDeps, got %d", len(target.PlatformDeps))
	}

	linuxDeps, exists := target.PlatformDeps["linux"]
	if !exists {
		t.Fatalf("Linux not found in PlatformDeps")
	}
	if len(linuxDeps.ArchDeps) != 2 {
		t.Errorf("Expected 2 architectures for Linux in PlatformDeps, got %d", len(linuxDeps.ArchDeps))
	}
}
