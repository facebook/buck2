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
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"unicode"
)

// GPT-4 implementation of Python's shlex.split
func shellSplit(input string) ([]string, error) {
	var result []string
	reader := strings.NewReader(input)
	scanner := bufio.NewScanner(reader)
	scanner.Split(bufio.ScanWords)
	var token strings.Builder
	inQuotes := false
	var quoteChar rune
	appendToken := func() {
		if token.Len() > 0 {
			result = append(result, token.String())
			token.Reset()
		}
	}
	for scanner.Scan() {
		word := scanner.Text()
		for _, r := range word {
			switch {
			case r == '\'' || r == '"':
				if inQuotes {
					if r == quoteChar {
						inQuotes = false
						quoteChar = 0
						appendToken()
						continue
					}
				} else {
					inQuotes = true
					quoteChar = r
					continue
				}
			case unicode.IsSpace(r):
				if !inQuotes {
					appendToken()
					continue
				}
			}
			token.WriteRune(r)
		}
		if !inQuotes {
			appendToken()
		}
	}
	if inQuotes {
		return nil, fmt.Errorf("unclosed quote in input: %s", input)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return result, nil
}

// A copy of https://github.com/golang/go/blob/go1.23.0/src/cmd/internal/quoted/quoted.go#L65
func join(args []string) (string, error) {
	var buf []byte
	for i, arg := range args {
		if i > 0 {
			buf = append(buf, ' ')
		}
		var sawSpace, sawSingleQuote, sawDoubleQuote bool
		for _, c := range arg {
			switch {
			case c > unicode.MaxASCII:
				continue
			case isSpaceByte(byte(c)):
				sawSpace = true
			case c == '\'':
				sawSingleQuote = true
			case c == '"':
				sawDoubleQuote = true
			}
		}
		switch {
		case !sawSpace && !sawSingleQuote && !sawDoubleQuote:
			buf = append(buf, arg...)

		case !sawSingleQuote:
			buf = append(buf, '\'')
			buf = append(buf, arg...)
			buf = append(buf, '\'')

		case !sawDoubleQuote:
			buf = append(buf, '"')
			buf = append(buf, arg...)
			buf = append(buf, '"')

		default:
			return "", fmt.Errorf("argument %q contains both single and double quotes and cannot be quoted", arg)
		}
	}
	return string(buf), nil
}

// A copy of https://github.com/golang/go/blob/go1.23.0/src/cmd/internal/quoted/quoted.go#L15
func isSpaceByte(c byte) bool {
	return c == ' ' || c == '\t' || c == '\n' || c == '\r'
}

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
	var wrappedBinary = flag.String("go", "", "wrapped go binary")
	var goRoot = flag.String("goroot", "", "go root")
	var outputFile = flag.String("output", "", "file to redirect stdout to")
	var workdir = flag.String("workdir", "", "directory to run the command in")
	var useFakeGoroot = flag.Bool("use-fake-goroot", false, "use a fake GOROOT")
	flag.Parse()
	unknownArgs := flag.Args()

	if *wrappedBinary == "" {
		log.Fatal("No wrapped binary specified")
	}

	absWrappedBinary, err := filepath.Abs(*wrappedBinary)
	if err != nil {
		log.Fatal("Failed to resolve wrapped binary: %s", err)
	}

	envs := make(map[string]string)
	for _, e := range os.Environ() {
		pair := strings.SplitN(e, "=", 2)
		envs[pair[0]] = pair[1]
	}

	goroot := *goRoot
	if goroot == "" {
		goroot = envs["GOROOT"]
	}

	if goroot != "" {
		absGoroot, err := filepath.Abs(goroot)
		if err != nil {
			log.Fatal("Failed to resolve GOROOT: %s", err)
		}
		envs["GOROOT"] = absGoroot
	}

	if buckScratchPath, ok := envs["BUCK_SCRATCH_PATH"]; ok {
		absBuckScratchPath, err := filepath.Abs(buckScratchPath)
		if err != nil {
			log.Fatal("Failed to resolve BUCK_SCRATCH_PATH: %s", err)
		}
		envs["GOCACHE"] = absBuckScratchPath

		if *useFakeGoroot {
			envs["GOROOT"] = absBuckScratchPath
		}
	}

	cwd, err := os.Getwd()
	if err != nil {
		log.Fatal("Failed to get current working directory: %s", err)
	}

	for _, envVar := range []string{"CC", "CGO_CFLAGS", "CGO_CPPFLAGS", "CGO_LDFLAGS"} {
		if value, ok := envs[envVar]; ok {
			//  HACK: Split the value into a list of arguments then join them back.
			//  This is because buck encodes quoted args in a way `go` doesn't like,
			//  but `join` does it in a way that `go` expects.
			splitValue := strings.Split(value, "\t")
			joinedValue, err := join(splitValue)
			if err != nil {
				log.Fatal("Failed to join %q: %s", envVar, err)
			}
			// HACK: Replace %cwd% with the current working directory to make it work when `go` does `cd` to a tmp-dir.
			envs[envVar] = strings.ReplaceAll(joinedValue, "%cwd%", cwd)
		}
	}
	for i, arg := range unknownArgs {
		unknownArgs[i] = strings.ReplaceAll(arg, "%cwd%", cwd)
	}

	var output *os.File
	if *outputFile == "" {
		output = os.Stdout
	} else {
		output, err = os.Create(*outputFile)
		if err != nil {
			log.Fatalf("Error creating output file: %s", err)
			os.Exit(1)
		}
		defer output.Close()
	}

	cmd := exec.Command(absWrappedBinary, unknownArgs...)

	cmd.Env = make([]string, 0, len(envs)/2)
	for k, v := range envs {
		cmd.Env = append(cmd.Env, k+"="+v)
	}

	if *workdir != "" {
		cmd.Dir = *workdir
	}

	cmd.Stdout = output
	cmd.Stderr = os.Stderr
	err = cmd.Run()
	if err != nil {
		exitCode := 1
		if exitErr, ok := err.(*exec.ExitError); ok {
			exitCode = exitErr.ExitCode()
		}
		fmt.Fprintln(os.Stderr, "Error running command:", err)
		os.Exit(exitCode)
	}
}
