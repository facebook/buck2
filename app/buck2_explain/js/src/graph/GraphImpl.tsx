/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useRef, useState} from 'react'
import ForceGraph2D, {LinkObject, NodeObject} from 'react-force-graph-2d'
import {Build} from '../fbs/explain'
import {RuleTypeDropdown} from './RuleTypeDropdown'
import {Node} from './GraphView'

// Here it goes everything that should reload on user interaction
export function GraphImpl(props: {
  nodeMap: Map<number, Node>
  build: Build
  categoryOptions: {category: string; count: number; checked: boolean}[]
}) {
  const {nodeMap, build, categoryOptions} = props

  const [categories, setCategories] = useState(categoryOptions)

  const toggleCategory = (id: number) => {
    const c = [...categories]
    c[id].checked = !c[id].checked
    setCategories(c)
  }

  const activeCategories = categories.filter(v => v.checked).map(v => v.category)

  for (const [k, node] of nodeMap) {
    const target = build.targets(k)!
    node.allow = activeCategories.includes(target.type()!)
    // Reset allowed deps
    node.allowedDeps.clear()
  }

  // Always set root node
  nodeMap.get(0)!.allow = true

  let filteredNodes = new Map()
  for (const [k, node] of nodeMap) {
    if (node.allow) {
      filteredNodes.set(k, node)
    }
  }

  // For each node A that goes, traverse the graph bottom up
  // until another node that goes is found, then add node A as allowedDep
  for (const [k, node] of filteredNodes) {
    let visited = new Set()
    let stack = [...node.rdeps]
    while (stack.length > 0) {
      const n1 = stack.pop()

      if (visited.has(n1)) {
        continue
      }
      visited.add(n1)

      if (nodeMap.get(n1)!.allow) {
        nodeMap.get(n1)!.allowedDeps.add(k)
      } else {
        stack = stack.concat(nodeMap.get(n1)!.rdeps)
      }
    }
  }

  // Build graph in a format that the graph library understands
  const data: NodeObject[] = []
  const edges: LinkObject[] = []

  for (const [k, _] of filteredNodes) {
    const target = build.targets(k)!

    // Add nodes to graph
    let options = {}

    data.push({
      ...options,
      id: k,
      name: target.configuredTargetLabel()!,
    })
  }

  for (const [k, node] of filteredNodes) {
    // Add edges
    for (const d of node.allowedDeps) {
      if (filteredNodes.has(d)) {
        edges.push({
          source: k,
          target: d,
          color: 'rgba(20, 20, 20, 0.5)',
        })
      }
    }
  }

  const graphRef = useRef<any>(null)

  return (
    <>
      <RuleTypeDropdown options={categories} handleCheckboxChange={toggleCategory} />
      <ForceGraph2D
        ref={graphRef}
        graphData={{nodes: data, links: edges}}
        onNodeClick={(node, _event) => console.log(node.nodeVal, 'click!')}
        onEngineTick={graphRef?.current?.zoomToFit}
        // cooldown + warmup ticks make the graph render already in its final form
        cooldownTicks={1}
        enableNodeDrag={false}
        warmupTicks={filteredNodes.size / 4}
        // looks
        linkDirectionalArrowLength={5 / Math.pow(filteredNodes.size, 0.2)}
        linkDirectionalArrowRelPos={1}
        linkCurvature={0.2}
        linkWidth={3 / Math.pow(filteredNodes.size, 0.5)}
      />
    </>
  )
}
