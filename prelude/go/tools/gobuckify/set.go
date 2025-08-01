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

import "slices"

type stringSet map[string]struct{}

func newSet() *stringSet {
	newSet := make(stringSet)
	return &newSet
}

func newFromList(vs []string) *stringSet {
	newSet := newSet()
	newSet.AddList(vs)
	return newSet
}

func (s *stringSet) Add(v string) {
	(*s)[v] = struct{}{}
}

func (s *stringSet) AddList(vs []string) {
	for _, v := range vs {
		(*s)[v] = struct{}{}
	}
}

func (s *stringSet) Remove(v string) {
	delete((*s), v)
}

func (s *stringSet) Len() int {
	return len((*s))
}

func (s *stringSet) SortedList() []string {
	result := make([]string, 0, len((*s)))
	for k := range *s {
		result = append(result, k)
	}
	slices.Sort(result)
	return result
}
