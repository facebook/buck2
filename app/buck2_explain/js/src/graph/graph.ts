/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

type Graph = Map<number, number[]>

interface Data {
  distance: number
  previous: number | null
}

export function shortestPathTree(graph: Graph, source: number): Graph {
  return pathTree(graph, source, 1)
}

export function longestPathTree(graph: Graph, source: number): Graph {
  return pathTree(graph, source, -1)
}

function pathTree(graph: Graph, source: number, weight: number): Graph {
  const distances: Map<number, Data> = new Map()
  // Initialize distances to all vertices as infinite, except the source
  for (const [k, _] of graph) {
    distances.set(k, {distance: Infinity, previous: null})
  }
  distances.set(source, {distance: 0, previous: null})
  // Traverse all vertices in topologically sorted order
  const sortedVertices = toposort(graph)
  for (const u of sortedVertices) {
    const deps = graph.get(u)!
    if (deps && distances.get(u)!.distance !== Infinity) {
      for (const v of deps) {
        // Update distance if current is shorter
        const newDistance = distances.get(u)!.distance + weight
        if (newDistance < distances.get(v)!.distance) {
          distances.set(v, {distance: newDistance, previous: u})
        }
      }
    }
  }

  // Go through all .previous to create tree
  let resGraph = new Map()
  for (const [k, _] of distances) {
    resGraph.set(k, [])
  }
  for (const [k, data] of distances) {
    if (data.previous == null) {
      continue
    }
    resGraph.get(data.previous)!.push(k)
  }
  return resGraph
}

export function toposort(graph: Graph): number[] {
  const inDegree: Map<number, number> = new Map()
  const zeroInDegreeQueue: number[] = []
  const topOrder: number[] = []
  // Initialize inDegree to 0 for all vertices
  for (const node of graph.keys()) {
    inDegree.set(node, 0)
  }
  // Calculate in-degree of each node
  for (const edges of graph.values()) {
    for (const edge of edges) {
      inDegree.set(edge, (inDegree.get(edge) || 0) + 1)
    }
  }
  // Enqueue all vertices with in-degree 0
  for (const [node, degree] of inDegree) {
    if (degree === 0) {
      zeroInDegreeQueue.push(node)
    }
  }
  // Process the queue
  while (zeroInDegreeQueue.length) {
    const node = zeroInDegreeQueue.shift()!
    topOrder.push(node)
    // For each adjacent vertex, reduce the in-degree
    // If in-degree becomes zero, add it to the queue
    const nodeEdges = graph.get(node) || []
    for (const edge of nodeEdges) {
      const updatedInDegree = (inDegree.get(edge) || 0) - 1
      inDegree.set(edge, updatedInDegree)
      if (updatedInDegree === 0) {
        zeroInDegreeQueue.push(edge)
      }
    }
  }
  // Check if there was a cycle in the graph
  if (topOrder.length !== graph.size) {
    throw new Error('There might be a cycle in the graph: ' + topOrder.length)
  }
  return topOrder
}
