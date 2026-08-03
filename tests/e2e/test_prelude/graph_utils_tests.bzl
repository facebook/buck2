# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:asserts.bzl", "asserts")
load("@prelude//utils:graph_utils.bzl", "find_cycle", "post_order_traversal", "pre_order_traversal")

def assert_cycle(expected_nodes, cycle):
    asserts.true(cycle != None)
    for node in expected_nodes:
        asserts.true(node in cycle)

def positions(graph, order):
    asserts.equals(len(graph), len(order), "not every node was ordered")
    result = {}
    for i, node in enumerate(order):
        asserts.true(node not in result, "{} appears more than once in {}".format(node, order))
        result[node] = i
    return result

def assert_pre_ordered(graph):
    order = pre_order_traversal(graph)
    at = positions(graph, order)
    for node, deps in graph.items():
        for dep in deps:
            asserts.true(at[node] < at[dep], "expected {} before {} in {}".format(node, dep, order))

def assert_post_ordered(graph):
    order = post_order_traversal(graph)
    at = positions(graph, order)
    for node, deps in graph.items():
        for dep in deps:
            asserts.true(at[dep] < at[node], "expected {} before {} in {}".format(dep, node, order))

def test_find_cycle():
    asserts.equals(None, find_cycle({}))
    asserts.equals(None, find_cycle({0: [1], 1: [2], 2: [3], 3: []}))
    assert_cycle([0, 0, 1, 2], find_cycle({0: [1], 1: [2], 2: [0]}))

# Verify that pre and post-order traversals work correctly, including when deps are duplicated.
_PRE_POST_ORDER_TESTS = [
    {0: [1], 1: [2], 2: [3], 3: []},
    {0: [1, 2], 1: [3], 2: [3], 3: []},
    {0: [2], 1: [2, 2], 2: []},
    {1: [2, 2], 0: [2], 2: []},
    {0: [3, 3, 3], 1: [3], 2: [3], 3: []},
    {0: [2, 2], 1: [2, 2], 2: []},
    {0: [], 1: [0], 2: [1, 0, 1], 3: [1, 2, 0, 2, 1]},
]

def test_pre_order_traversal():
    asserts.equals([], pre_order_traversal({}))
    for graph in _PRE_POST_ORDER_TESTS:
        assert_pre_ordered(graph)

def test_post_order_traversal():
    asserts.equals([], post_order_traversal({}))
    for graph in _PRE_POST_ORDER_TESTS:
        assert_post_ordered(graph)
