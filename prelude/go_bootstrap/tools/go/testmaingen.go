/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Generates testmain.go files from a group of test files.
//
// Regular go tests (ones run with `go test`) don't actually define a main
// package. Moreover, Go's reflection does not have the ability to inspect
// packages (e.g. list functions). This script generates a main.go that
// runs some set of tests passed in on the CLI. The code liberally borrows from
// the `go test` implementation at https://github.com/golang/go/blob/master/src/cmd/go/test.go

package main

import (
	"bufio"
	"bytes"
	"errors"
	"flag"
	"fmt"
	"go/ast"
	"go/doc"
	"go/parser"
	"go/token"
	"log"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"text/template"
	"unicode"
	"unicode/utf8"
)

// A map of: pkg -> [file name -> var name]
type coverPkgFlag map[string]map[string]string

func (c coverPkgFlag) String() string {
	var buffer bytes.Buffer
	for k, vs := range c {
		if len(vs) > 0 {
			buffer.WriteString(k)
			buffer.WriteString(":")

			first := true
			for f, v := range vs {
				buffer.WriteString(f)
				buffer.WriteString("=")
				buffer.WriteString(v)
				if !first {
					buffer.WriteString(",")
				}
				first = false
			}

			buffer.WriteString(";")
		}
	}
	return buffer.String()
}

func (c coverPkgFlag) Set(value string) error {
	for _, path := range strings.Split(value, ";") {
		pkgAndFiles := strings.Split(path, ":")
		if len(pkgAndFiles) != 2 {
			return errors.New("bad format: expected path1:file1=var1,file2=var2;path2:file3=var3,file4=var4")
		}
		pkg := pkgAndFiles[0]
		for _, fileAndVar := range strings.Split(pkgAndFiles[1], ",") {
			fileAndVar := strings.Split(fileAndVar, "=")
			if len(fileAndVar) != 2 {
				return errors.New("bad format: expected path1:file1=var1,file2=var2;path2:file3=var3,file4=var4")
			}

			if c[pkg] == nil {
				c[pkg] = make(map[string]string)
			}

			c[pkg][fileAndVar[0]] = fileAndVar[1]
		}
	}
	return nil
}

// Flags
var (
	pkgImportPath string
	outputFile    string
	testCoverMode string
	coverPkgs     = make(coverPkgFlag)
)

func init() {
	flag.StringVar(&pkgImportPath, "import-path", "test", "The import path in the test file")
	flag.StringVar(&outputFile, "output", "", "The path to the output file. Default to stdout.")
	flag.Var(coverPkgs, "cover-pkgs", "List of packages & coverage variables to gather coverage info on, in the form of IMPORT-PATH1:file1=var1,file2=var2,file3=var3;IMPORT-PATH2:...")
	flag.StringVar(&testCoverMode, "cover-mode", "", "Cover mode (see `go tool cover`)")
}

// Resolve argsfiles in args (e.g. `@file.txt`).
func loadArgs(args []string) []string {
	newArgs := make([]string, 0, 0)
	for _, arg := range args {
		if !strings.HasPrefix(arg, "@") {
			newArgs = append(newArgs, arg)
		} else {
			file, _ := os.Open(arg[1:])
			defer file.Close()
			scanner := bufio.NewScanner(file)
			for scanner.Scan() {
				newArgs = append(newArgs, scanner.Text())
			}
		}
	}
	return newArgs
}

func main() {
	os.Args = loadArgs(os.Args)
	flag.Parse()

	coverInfos := make([]coverInfo, 0, len(coverPkgs))
	pkgs := make([]*Package, 0, len(coverPkgs))
	testCoverPaths := make([]string, 0, len(coverPkgs))
	for importPath, fileToVarMap := range coverPkgs {
		coverVarMap := make(map[string]*CoverVar, len(fileToVarMap))
		for fileName, varName := range fileToVarMap {
			coverVarMap[fileName] = &CoverVar{
				File: filepath.Join(importPath, fileName),
				Var:  varName,
			}
		}

		pkg := &Package{ImportPath: importPath}
		cover := coverInfo{
			Package: pkg,
			Vars:    coverVarMap,
		}
		pkgs = append(pkgs, pkg)
		coverInfos = append(coverInfos, cover)
		testCoverPaths = append(testCoverPaths, importPath)
	}

	testFuncs, err := loadTestFuncsFromFiles(pkgImportPath, flag.Args())
	if err != nil {
		log.Fatalln("Could not read test files:", err)
	}
	// Coverage enabled
	if testCoverMode != "" {
		testFuncs.Cover = &TestCover{
			Pkgs:  pkgs,
			Vars:  coverInfos,
			Paths: testCoverPaths,
			Mode:  testCoverMode,
		}
	}

	out := os.Stdout
	if outputFile != "" {
		out, err = os.Create(outputFile)
		if err != nil {
			log.Fatalln("Could not write test main:", err)
		}
	}

	if err := testmainTmplNewCoverage.Execute(out, testFuncs); err != nil {
		log.Fatalln("Failed to generate main file:", err)
	}
}

func loadTestFuncsFromFiles(packageImportPath string, files []string) (*testFuncs, error) {
	t := &testFuncs{
		Package: &Package{
			ImportPath: packageImportPath,
		},
	}
	for _, filename := range files {
		if err := t.load(filename, "_test", &t.ImportTest, &t.NeedTest); err != nil {
			return nil, err
		}
	}
	return t, nil
}

//
//
//
// Most of the code below is a direct copy from the 'go test' command:
//   https://github.com/golang/go/blob/release-branch.go1.24/src/cmd/go/internal/load/test.go
//
//
//

// This is a fake version of the actual Package type, since we don't really need all
// ~300 fields of it.
type Package struct {
	ImportPath string `json:",omitempty"` // import path of package in dir
}

// isTestFunc tells whether fn has the type of a testing function. arg
// specifies the parameter type we look for: B, F, M or T.
func isTestFunc(fn *ast.FuncDecl, arg string) bool {
	if fn.Type.Results != nil && len(fn.Type.Results.List) > 0 ||
		fn.Type.Params.List == nil ||
		len(fn.Type.Params.List) != 1 ||
		len(fn.Type.Params.List[0].Names) > 1 {
		return false
	}
	ptr, ok := fn.Type.Params.List[0].Type.(*ast.StarExpr)
	if !ok {
		return false
	}
	// We can't easily check that the type is *testing.M
	// because we don't know how testing has been imported,
	// but at least check that it's *M or *something.M.
	// Same applies for B, F and T.
	if name, ok := ptr.X.(*ast.Ident); ok && name.Name == arg {
		return true
	}
	if sel, ok := ptr.X.(*ast.SelectorExpr); ok && sel.Sel.Name == arg {
		return true
	}
	return false
}

// isTest tells whether name looks like a test (or benchmark, according to prefix).
// It is a Test (say) if there is a character after Test that is not a lower-case letter.
// We don't want TesticularCancer.
func isTest(name, prefix string) bool {
	if !strings.HasPrefix(name, prefix) {
		return false
	}
	if len(name) == len(prefix) { // "Test" is ok
		return true
	}
	rune, _ := utf8.DecodeRuneInString(name[len(prefix):])
	return !unicode.IsLower(rune)
}

type TestCover struct {
	Mode  string
	Local bool
	Pkgs  []*Package
	Paths []string
	Vars  []coverInfo
}

// CoverVar holds the name of the generated coverage variables targeting the named file.
type CoverVar struct {
	File string // local file name
	Var  string // name of count struct
}

type coverInfo struct {
	Package *Package
	Vars    map[string]*CoverVar
}

type testFuncs struct {
	Tests       []testFunc
	Benchmarks  []testFunc
	FuzzTargets []testFunc
	Examples    []testFunc
	TestMain    *testFunc
	Package     *Package
	ImportTest  bool
	NeedTest    bool
	ImportXtest bool
	NeedXtest   bool
	Cover       *TestCover
}

// ImportPath returns the import path of the package being tested, if it is within GOPATH.
// This is printed by the testing package when running benchmarks.
func (t *testFuncs) ImportPath() string {
	pkg := t.Package.ImportPath
	if strings.HasPrefix(pkg, "_/") {
		return ""
	}
	if pkg == "command-line-arguments" {
		return ""
	}
	return pkg
}

// Covered returns a string describing which packages are being tested for coverage.
// If the covered package is the same as the tested package, it returns the empty string.
// Otherwise it is a comma-separated human-readable list of packages beginning with
// " in", ready for use in the coverage message.
func (t *testFuncs) Covered() string {
	if t.Cover == nil || t.Cover.Paths == nil {
		return ""
	}
	return " in " + strings.Join(t.Cover.Paths, ", ")
}

func (t *testFuncs) CoverSelectedPackages() string {
	if t.Cover == nil || t.Cover.Paths == nil {
		return `[]string{"` + t.Package.ImportPath + `"}`
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "[]string{")
	for k, p := range t.Cover.Pkgs {
		if k != 0 {
			sb.WriteString(", ")
		}
		fmt.Fprintf(&sb, `"%s"`, p.ImportPath)
	}
	sb.WriteString("}")
	return sb.String()
}

type testFunc struct {
	Package   string // imported package name (_test or _xtest)
	Name      string // function name
	Output    string // output, for examples
	Unordered bool   // output is allowed to be unordered.
}

var testFileSet = token.NewFileSet()

func (t *testFuncs) load(filename, pkg string, doImport, seen *bool) error {
	// Pass in the overlaid source if we have an overlay for this file.
	src, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer src.Close()
	f, err := parser.ParseFile(testFileSet, filename, src, parser.ParseComments|parser.SkipObjectResolution)
	if err != nil {
		return err
	}
	for _, d := range f.Decls {
		n, ok := d.(*ast.FuncDecl)
		if !ok {
			continue
		}
		if n.Recv != nil {
			continue
		}
		name := n.Name.String()
		switch {
		case name == "TestMain":
			if isTestFunc(n, "T") {
				t.Tests = append(t.Tests, testFunc{pkg, name, "", false})
				*doImport, *seen = true, true
				continue
			}
			err := checkTestFunc(n, "M")
			if err != nil {
				return err
			}
			if t.TestMain != nil {
				return errors.New("multiple definitions of TestMain")
			}
			t.TestMain = &testFunc{pkg, name, "", false}
			*doImport, *seen = true, true
		case isTest(name, "Test"):
			err := checkTestFunc(n, "T")
			if err != nil {
				return err
			}
			t.Tests = append(t.Tests, testFunc{pkg, name, "", false})
			*doImport, *seen = true, true
		case isTest(name, "Benchmark"):
			err := checkTestFunc(n, "B")
			if err != nil {
				return err
			}
			t.Benchmarks = append(t.Benchmarks, testFunc{pkg, name, "", false})
			*doImport, *seen = true, true
		case isTest(name, "Fuzz"):
			err := checkTestFunc(n, "F")
			if err != nil {
				return err
			}
			t.FuzzTargets = append(t.FuzzTargets, testFunc{pkg, name, "", false})
			*doImport, *seen = true, true
		}
	}
	ex := doc.Examples(f)
	sort.Slice(ex, func(i, j int) bool { return ex[i].Order < ex[j].Order })
	for _, e := range ex {
		*doImport = true // import test file whether executed or not
		if e.Output == "" && !e.EmptyOutput {
			// Don't run examples with no output.
			continue
		}
		t.Examples = append(t.Examples, testFunc{pkg, "Example" + e.Name, e.Output, e.Unordered})
		*seen = true
	}
	return nil
}

func checkTestFunc(fn *ast.FuncDecl, arg string) error {
	var why string
	if !isTestFunc(fn, arg) {
		why = fmt.Sprintf("must be: func %s(%s *testing.%s)", fn.Name.String(), strings.ToLower(arg), arg)
	}
	if fn.Type.TypeParams.NumFields() > 0 {
		why = "test functions cannot have type parameters"
	}
	if why != "" {
		pos := testFileSet.Position(fn.Pos())
		return fmt.Errorf("%s: wrong signature for %s, %s", pos, fn.Name.String(), why)
	}
	return nil
}

var testmainTmplNewCoverage = template.Must(template.New("main").Parse(`
// Code generated by 'go test'. DO NOT EDIT.

package main

import (
	"fmt"
	"os"
	"path/filepath"
{{if .TestMain}}
	"reflect"
{{end}}
	"testing"
	"testing/internal/testdeps"
{{if .Cover}}
	"internal/coverage/cfile"
{{end}}

{{if .ImportTest}}
	{{if .NeedTest}}_test{{else}}_{{end}} {{.Package.ImportPath | printf "%q"}}
{{end}}
{{if .ImportXtest}}
	{{if .NeedXtest}}_xtest{{else}}_{{end}} {{.Package.ImportPath | printf "%s_test" | printf "%q"}}
{{end}}
)

var tests = []testing.InternalTest{
{{range .Tests}}
	{"{{.Name}}", {{.Package}}.{{.Name}}},
{{end}}
}

var benchmarks = []testing.InternalBenchmark{
{{range .Benchmarks}}
	{"{{.Name}}", {{.Package}}.{{.Name}}},
{{end}}
}

var fuzzTargets = []testing.InternalFuzzTarget{
{{range .FuzzTargets}}
	{"{{.Name}}", {{.Package}}.{{.Name}}},
{{end}}
}

var examples = []testing.InternalExample{
{{range .Examples}}
	{"{{.Name}}", {{.Package}}.{{.Name}}, {{.Output | printf "%q"}}, {{.Unordered}}},
{{end}}
}

func init() {
{{if .Cover}}
	testdeps.CoverMode = {{printf "%q" .Cover.Mode}}
	testdeps.Covered = {{printf "%q" .Covered}}
	testdeps.CoverSelectedPackages = {{printf "%s" .CoverSelectedPackages}}
	testdeps.CoverSnapshotFunc = cfile.Snapshot
	testdeps.CoverProcessTestDirFunc = cfile.ProcessCoverTestDir
	testdeps.CoverMarkProfileEmittedFunc = cfile.MarkProfileEmitted

{{end}}
	testdeps.ImportPath = {{.ImportPath | printf "%q"}}
}

func main() {
	// Buck ensures that resources defined on the test targets live in the same
	// directory as the binary. We change the working directory to this
	// directory to make sure that tests can read test fixtures relative to the
	// current working directory. This matches behavior with "go test" from the
	// test author perspective.
	execPath, err := os.Executable()
	if err != nil {
		os.Stderr.WriteString("Unable to get path to test binary executable.")
		os.Exit(1)
	}
	execDir := filepath.Dir(execPath)
	err = os.Chdir(execDir)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to change directory to %s.", execDir)
		os.Exit(1)
	}
	m := testing.MainStart(testdeps.TestDeps{}, tests, benchmarks, fuzzTargets, examples)
{{with .TestMain}}
	{{.Package}}.{{.Name}}(m)
	os.Exit(int(reflect.ValueOf(m).Elem().FieldByName("exitCode").Int()))
{{else}}
	os.Exit(m.Run())
{{end}}
}

`))
