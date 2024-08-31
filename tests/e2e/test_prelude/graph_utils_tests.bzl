# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:asserts.bzl", "asserts")
load("@prelude//utils:graph_utils.bzl", "find_cycle")

def assert_cycle(expected_nodes, cycle):
    asserts.true(cycle != None)
    for node in expected_nodes:
        asserts.true(node in cycle)

def test_find_cycle():
    asserts.equals(None, find_cycle({}))
    asserts.equals(None, find_cycle({0: [1], 1: [2], 2: [3], 3: []}))
    assert_cycle([0, 0, 1, 2], find_cycle({0: [1], 1: [2], 2: [0]}))
