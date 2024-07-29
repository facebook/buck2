/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useContext, useRef, useState} from 'react'
import {Target} from './Target'
import {DataContext} from './App'
import ForceGraph2D, {LinkObject, NodeObject} from 'react-force-graph-2d'
import {Build} from './fbs/explain'

type Node = {
  value: number
  deps: number[]
  rdeps: number[]
  allowedDeps: Set<number>
  allow: boolean
  leave: boolean
}

function defaultNode(): Node {
  return {
    value: 0,
    deps: [],
    rdeps: [],
    allowedDeps: new Set(),
    allow: false,
    leave: false,
  }
}

export function GraphView(props: {view: string}) {
  const {build, allTargets} = useContext(DataContext)
  if (build == null) {
    // TODO: this should show a loading sign
    return null
  }

  // build better data structure
  let nodeMap = new Map<number, Node>()
  for (let i = 0; i < build.targetsLength(); i++) {
    const target = build.targets(i)!

    // Create node object
    if (nodeMap.get(i) == null) {
      nodeMap.set(i, {
        ...defaultNode(),
        value: i,
      })
    }
  }

  // Set options
  for (const [k, node] of nodeMap) {
    const target = build.targets(k)!

    // Record if leave
    node.leave = target.depsLength() === 0

    for (let i = 0; i < target.depsLength(); i++) {
      const dep = target.deps(i)!
      const d = allTargets[dep]

      // Record deps
      node.deps.push(d)

      // Record rdeps
      if (d === k) {
        throw Error('wth')
      }
      nodeMap.get(d)!.rdeps.push(k)
    }
  }

  let categoriesCounter = new Map()
  for (const [k, node] of nodeMap) {
    const target = build.targets(k)
    const type = target!.type()
    categoriesCounter.set(type, (categoriesCounter.get(type) ?? 0) + 1)
  }

  let categoryOptions: {category: string; count: number; checked: false}[] = []
  for (const [category, count] of categoriesCounter) {
    categoryOptions.push({category, count, checked: false})
  }

  return <GraphImpl nodeMap={nodeMap} build={build} categoryOptions={categoryOptions} />
}

// Here it goes everything that should reload on user interaction
function GraphImpl(props: {
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
        warmupTicks={filteredNodes.size / 4}
        // looks
        linkDirectionalArrowLength={3.5}
        linkDirectionalArrowRelPos={1}
        linkCurvature={0.2}
        linkWidth={2}
      />
    </>
  )
}

function RuleTypeDropdown(props: {
  options: {category: string; checked: boolean; count: number}[]
  handleCheckboxChange: (i: number) => void
}) {
  const [dropdownActive, setDropdownActive] = useState(false)

  return (
    <div className={'dropdown ' + (dropdownActive ? 'is-active' : '')}>
      <div className="dropdown-trigger">
        <button
          className="button"
          aria-haspopup="true"
          aria-controls="dropdown-menu"
          onClick={() => setDropdownActive(!dropdownActive)}>
          <span>Select rule types</span>
          <span className="icon is-small">
            <i className="fas fa-angle-down" aria-hidden="true"></i>
          </span>
        </button>
      </div>
      <div className="dropdown-menu" id="dropdown-menu" role="menu">
        <div className="dropdown-content">
          {props.options.map((v, index: number) => (
            <CheckboxItem
              key={index}
              label={v.category}
              count={v.count}
              checked={v.checked}
              handleCheckboxChange={() => props.handleCheckboxChange(index)}
            />
          ))}
        </div>
      </div>
    </div>
  )
}

function CheckboxItem(props: {
  label: string
  count: number
  checked: boolean
  handleCheckboxChange: () => void
}) {
  return (
    <div className="dropdown-item">
      <label className="checkbox">
        <input
          checked={props.checked}
          type="checkbox"
          onChange={() => props.handleCheckboxChange()}
        />
        {' ' + props.label + ' (' + props.count + ')'}
      </label>
    </div>
  )
}
