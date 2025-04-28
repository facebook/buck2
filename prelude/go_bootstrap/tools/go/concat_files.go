/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package main

import (
	"flag"
	"io"
	"log"
	"os"
)

func main() {
	var outputFile = flag.String("output", "", "help message for flag n")
	flag.Parse()
	inputFiles := flag.Args()

	if *outputFile == "" || len(inputFiles) < 2 {
		log.Fatal("usage: concat_files.go --output out.txt in1.txt in2.txt")
	}

	f, err := os.Create(*outputFile)
	if err != nil {
		log.Fatal(os.Stderr, "Error creating output file: %v", err)
	}
	defer f.Close()

	for _, file := range inputFiles {
		infile, err := os.Open(file)
		if err != nil {
			log.Fatal(os.Stderr, "Error opening input file %s: %v", file, err)
		}
		defer infile.Close()

		_, err = io.Copy(f, infile)
		if err != nil {
			log.Fatal("Error copying file %s: %v\n", file, err)
		}
	}
}
