/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import {toposort, shortestPathTree, longestPathTree} from './graph'

function assert(condition, message) {
  if (!condition) {
    throw new Error(message || 'Assertion failed')
  }
}

function arraysEqual(a, b) {
  // Check if the arrays are the same length
  if (a.length !== b.length) {
    return false
  }
  // Check if all items exist and are in the same order
  for (let i = 0; i < a.length; i++) {
    if (Array.isArray(a[i]) && Array.isArray(b[i])) {
      // Recursively check for nested arrays
      if (!arraysEqual(a[i], b[i])) {
        return false
      }
    } else if (a[i] !== b[i]) {
      // Check if elements are different
      return false
    }
  }
  // Otherwise, return true
  return true
}

function testToposort() {
  const graph = new Map()

  graph.set(0, [1, 2])
  graph.set(1, [2])
  graph.set(2, [])

  const res = toposort(graph)
  assert(arraysEqual(res, [0, 1, 2]), 'assertion failed, value received: ' + res)
  console.log(arguments.callee.name, 'passed')
}

testToposort()

function testShortestTree() {
  const graph = new Map()

  graph.set(0, [1, 2])
  graph.set(1, [2])
  graph.set(2, [])

  const res = shortestPathTree(graph, 0)

  assert(arraysEqual(res.get(0), [1, 2]), 'assertion failed for node 0, value received: ' + res)
  assert(arraysEqual(res.get(1), []), 'assertion failed for node 1, value received: ' + res)
  assert(arraysEqual(res.get(2), []), 'assertion failed for node 2, value received: ' + res)
  console.log(arguments.callee.name, 'passed')
}

testShortestTree()

function testLongestTree() {
  const graph = new Map()

  graph.set(0, [1, 2])
  graph.set(1, [2])
  graph.set(2, [])

  const res = longestPathTree(graph, 0)

  assert(arraysEqual(res.get(0), [1]), 'assertion failed for node 0, value received: ' + res)
  assert(arraysEqual(res.get(1), [2]), 'assertion failed for node 1, value received: ' + res)
  assert(arraysEqual(res.get(2), []), 'assertion failed for node 2, value received: ' + res)
  console.log(arguments.callee.name, 'passed')
}

testLongestTree()
