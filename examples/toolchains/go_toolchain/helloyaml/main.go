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
	"fmt"
	"github.com/goccy/go-yaml"
)

func main() {
	var v struct {
		A int
		B string
	}
	v.A = 1
	v.B = "hello"
	bytes, err := yaml.Marshal(v)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(bytes))
}
