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
  nodes: Map<number, Node>
  build: Build
  categoryOptions: {category: string; count: number; checked: boolean}[]
}) {
  const {nodes, build, categoryOptions} = props

  const nodeMap = new Map()
  for (const [k, node] of nodes) {
    nodeMap.set(k, {...node, allowedDeps: new Set(), allow: false})
  }

  const [categories, setCategories] = useState(categoryOptions)
  const [includeContaining, setIncludeContaining] = useState<string[]>([])
  const [excludeContaining, setExcludeContaining] = useState<string[]>([])

  const toggleCategory = (id: number) => {
    const c = [...categories]
    c[id].checked = !c[id].checked
    setCategories(c)
  }

  const activeCategories = categories.filter(v => v.checked).map(v => v.category)

  for (const [k, node] of nodeMap) {
    const target = build.targets(k)!
    const label = target.configuredTargetLabel()!

    node.allow = activeCategories.includes(target.type()!)
    for (const v of includeContaining) {
      if (label.includes(v)) {
        node.allow = true
      }
    }
    for (const v of excludeContaining) {
      if (label.includes(v)) {
        node.allow = false
      }
    }
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

  function setIncludeExclude() {
    const inputValue = (id: string) => (document.getElementById(id) as HTMLInputElement).value

    const inc = inputValue('includeContaining')
    setIncludeContaining(inc ? inc.split(',') : [])
    const exc = inputValue('excludeContaining')
    setExcludeContaining(exc ? exc.split(',') : [])
  }

  const graphRef = useRef<any>(null)

  return (
    <>
      <div className="grid mt-4">
        <div className="cell">
          <div className="field">
            <label className="label">Filter by labels:</label>
            <div className="control">
              <input
                id="includeContaining"
                className="input"
                type="text"
                placeholder="Include containing"
              />
            </div>
            <div className="control">
              <input
                id="excludeContaining"
                className="input"
                type="text"
                placeholder="Exclude containing"
              />
            </div>
          </div>
          <button
            type="submit"
            onClick={setIncludeExclude}
            onPointerDown={setIncludeExclude}
            className="button is-dark">
            <span>Apply</span>
          </button>
        </div>
        <div className="cell">
          <div className="field">
            <label className="label">Include targets with rule types:</label>
            <RuleTypeDropdown options={categories} handleCheckboxChange={toggleCategory} />
          </div>
        </div>
      </div>
      <article className="message">
        <div className="message-body">
          Number of nodes: {data.length} <br />
          Number of edges: {edges.length}
        </div>
      </article>
      <ForceGraph2D
        ref={graphRef}
        graphData={{nodes: data, links: edges}}
        onNodeClick={(node, _event) => console.log(node.nodeVal, 'click!')}
        onNodeRightClick={(node, _event) => {
          const url = new URL(window.location.href)
          url.searchParams.set('target', node.name)
          url.searchParams.delete('graph')
          window.open(url.toString(), '_blank')
        }}
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
