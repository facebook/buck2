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
  allTargets: {[key: string]: number}
  categoryOptions: {category: string; count: number; checked: boolean}[]
}) {
  const {nodes, build, categoryOptions, allTargets} = props

  const nodeMap = new Map()
  for (const [k, node] of nodes) {
    nodeMap.set(k, {...node, allowedDeps: new Map(), allow: false})
  }

  const [categories, setCategories] = useState(categoryOptions)
  const [includeContaining, setIncludeContaining] = useState<string[]>([])
  const [excludeContaining, setExcludeContaining] = useState<string[]>([])
  const [pathFrom, setPathFrom] = useState<string>('')
  const [pathTo, setPathTo] = useState<string>('')

  const activeCategories = categories.filter(v => v.checked).map(v => v.category)

  // union of 'includes', minus 'excludes'
  for (const [k, node] of nodeMap) {
    const target = build.targets(k)!
    const label = target.configuredTargetLabel()!

    node.allow = activeCategories.includes(target.type()!)

    // Filter by label
    for (const v of includeContaining) {
      if (label.includes(v)) {
        node.allow = true
      }
    }

    // Some path
    if (pathFrom && pathTo && allTargets) {
      const from = allTargets[pathFrom]
      const to = allTargets[pathTo]
      const parentOf = new Map()
      parentOf.set(from, null)
      const queue = [from]

      while (queue.length > 0) {
        let node = queue.shift()!
        if (node === to) {
          break
        }
        for (let d of nodeMap.get(node)!.deps) {
          if (!parentOf.has(d)) {
            parentOf.set(d, node)
            queue.push(d)
          }
        }
      }

      // set allowed if in path
      let node = to
      while (node) {
        nodeMap.get(node)!.allow = true
        node = parentOf.get(node)
      }
    }

    // Exclude by label
    for (const v of excludeContaining) {
      if (label.includes(v)) {
        node.allow = false
      }
    }

    // Always set root node
    nodeMap.get(0)!.allow = true
  }

  let filteredNodes = new Map()
  for (const [k, node] of nodeMap) {
    if (node.allow) {
      filteredNodes.set(k, node)
    }
  }

  // For each node A that goes, traverse the graph bottom up BFS
  // until another node that goes is found, then add node A as allowedDep
  // Also stores shortest path length from last allowed to later add as edge label

  for (const [k, _] of filteredNodes) {
    let visited: Map<number, number> = new Map()
    visited.set(k, 0)
    let stack = [k]

    while (stack.length > 0) {
      const n1 = stack.shift()

      for (const r of nodeMap.get(n1)!.rdeps) {
        if (visited.has(r)) {
          continue
        }
        const distance = visited.get(n1)! + 1
        visited.set(r, distance)
        if (nodeMap.get(r)!.allow) {
          nodeMap.get(r)!.allowedDeps.set(k, distance)
        } else {
          stack.push(r)
        }
      }
    }
  }

  // Build graph in a format that the graph library understands
  const data: NodeObject[] = []
  const edges: LinkObject[] = []

  for (const [k, _] of filteredNodes) {
    const target = build.targets(k)!

    // Add nodes to graph
    data.push({
      id: k,
      name: target.configuredTargetLabel()!,
      group: target.configuredTargetLabel()!.split('#')[1],
    })
  }

  for (const [k, node] of filteredNodes) {
    // Add edges
    for (const [d, counter] of node.allowedDeps) {
      if (!filteredNodes.has(d)) {
        throw Error("this shouldn't be possible")
      }
      edges.push({
        source: k,
        target: d,
        name: `steps: ${counter}`,
        color: 'rgba(20, 20, 20, 0.5)',
      })
    }
  }

  const dagMode = edges.length / data.length > 3 ? 'td' : undefined

  function applyFilters() {
    const inputValue = (id: string) =>
      (document.getElementById(id) as HTMLInputElement).value.trim()

    // Include exclude by label
    const inc = inputValue('includeContaining')
    setIncludeContaining(inc ? inc.split(',') : [])
    const exc = inputValue('excludeContaining')
    setExcludeContaining(exc ? exc.split(',') : [])

    // Include by path
    setPathFrom(inputValue('pathFrom'))
    setPathTo(inputValue('pathTo'))

    // Include by rule type
    const checkboxes = document.querySelectorAll('#checkboxes input[type="checkbox"]')
    for (let i = 0; i < checkboxes.length; i++) {
      categories[i].checked = checkboxes[i].checked
    }
    setCategories([...categories])
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
        </div>
        <div className="cell">
          <div className="field">
            <label className="label">Some path:</label>
            <div className="control">
              <input id="pathFrom" className="input" type="text" placeholder="from" />
            </div>
            <div className="control">
              <input id="pathTo" className="input" type="text" placeholder="to" />
            </div>
          </div>
        </div>
        <div className="cell" id="checkboxes">
          <div className="field">
            <label className="label">Include targets with rule types:</label>
            <RuleTypeDropdown options={categories} />
          </div>
        </div>
        <div className="cell">
          <button
            type="submit"
            onClick={applyFilters}
            onPointerDown={applyFilters}
            className="button is-dark">
            <span>Apply filters</span>
          </button>
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
        onNodeClick={(node, _event) => console.log(node.name, 'click!')}
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
        warmupTicks={100}
        // looks
        linkDirectionalArrowLength={5 / Math.pow(filteredNodes.size, 0.2)}
        linkDirectionalArrowRelPos={1}
        linkCurvature={0.2}
        linkWidth={3 / Math.pow(filteredNodes.size, 0.5)}
        linkHoverPrecision={6}
        dagMode={dagMode}
        nodeAutoColorBy="group"
      />
    </>
  )
}
