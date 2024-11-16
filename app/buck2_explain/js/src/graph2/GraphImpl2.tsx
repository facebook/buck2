/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useRef, useState} from 'react'
import {Build} from '../fbs/explain'
import {RuleTypeDropdown} from './RuleTypeDropdown'
import {Node} from './GraphView2'
import {GraphViz2} from './GraphViz2'
import {LinkObject, NodeObject} from 'react-force-graph-2d'
import {formatTargetLabel} from '../formatTargetLabel'

enum DisplayType {
  rootNode,
  passesFilters,
  hidden,
  highlighted,
  actionsRan,
}

const displayTypeColors: {[key in DisplayType]: string} = {
  // https://coolors.co/1c77c3-39a9db-9ec1a3-cfe0c3-e9724c
  [DisplayType.rootNode]: '#1a181b',
  [DisplayType.passesFilters]: '#1c77c3',
  [DisplayType.highlighted]: '#e9724c',
  [DisplayType.actionsRan]: '#39a9db',
  [DisplayType.hidden]: 'gray', // doesn't matter
}

interface DisplayNode extends Node {
  allowedDeps: Map<number, number>
  displayType: DisplayType
}

function showNode(node: DisplayNode) {
  return node.displayType != DisplayType.hidden
}

// Here it goes everything that has to recompute on user interaction.
// On big graphs recomputing less matters
export function GraphImpl2(props: {
  nodes: Map<number, Node>
  build: Build
  allTargets: {[key: string]: number}
}) {
  const {nodes, build} = props

  const nodeMap: Map<number, DisplayNode> = new Map()
  for (const [k, node] of nodes) {
    nodeMap.set(k, {...node, allowedDeps: new Map(), displayType: DisplayType.hidden})
  }

  const [colorByCfg, setColorByCfg] = useState(false)
  const [showLabels, setShowLabels] = useState(false)
  const [includeContaining, setIncludeContaining] = useState<string[]>([])
  const [excludeContaining, setExcludeContaining] = useState<string[]>([])
  const [highlighted, setHighlighted] = useState<string | null>(null)

  // Intersection of 'includes', minus 'excludes'
  for (const [k, node] of nodeMap) {
    const target = build.targets(k)!
    const label = formatTargetLabel(target.label()!)

    // When null, means it wasn't affected by any of the filters and to use default
    let passesFilters = null

    // Filter by label
    if (includeContaining.length > 0) {
      let contains = false
      for (const v of includeContaining) {
        if (label.includes(v)) {
          contains = true
          break
        }
      }
      passesFilters = passesFilters !== false && contains
    }

    // Exclude by label
    for (const v of excludeContaining) {
      if (label.includes(v)) {
        passesFilters = false
      }
    }

    if (passesFilters === true) {
      node.displayType = DisplayType.passesFilters
    }

    // Add highlighted
    if (highlighted) {
      if (label.includes(highlighted)) {
        node.displayType = DisplayType.highlighted
      }
    }

    // Targets with actions ran
    if (target.actionsLength() > 0) {
      node.displayType = DisplayType.actionsRan
    }
  }

  // Always set root node
  nodeMap.get(0)!.displayType = DisplayType.rootNode

  let displayNodes: Map<number, DisplayNode> = new Map()
  let filteredNodes = new Map()
  for (const [k, node] of nodeMap) {
    if (showNode(node)) {
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
        if (showNode(nodeMap.get(r)!)) {
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

  for (const [k, node] of filteredNodes) {
    const target = build.targets(k)!

    // Add nodes to graph
    data.push({
      val: 0.5,
      id: k,
      name: formatTargetLabel(target.label()!),
      color: colorByCfg ? undefined : displayTypeColors[node.displayType],
      cfg: target.label()!.cfg()!,
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

  function applyFilters() {
    const inputValue = (id: string) =>
      (document.getElementById(id) as HTMLInputElement).value.trim()

    // Include exclude by label
    const inc = inputValue('includeContaining')
    setIncludeContaining(inc ? inc.split(',') : [])
    const exc = inputValue('excludeContaining')
    setExcludeContaining(exc ? exc.split(',') : [])

    // Highlight by label
    setHighlighted(inputValue('highlightNode'))
  }

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
            <label className="label">Highlight target by label:</label>
            <div className="control">
              <input
                id="highlightNode"
                className="input"
                type="text"
                placeholder="Label to highlight"
              />
            </div>
          </div>
        </div>
        <div className="cell">
          <button type="submit" onClick={applyFilters} className="button is-dark">
            <span>Apply filters</span>
          </button>
        </div>
        <div className="cell">
          <label className="checkbox ml-2 mt-4">
            <input
              type="checkbox"
              checked={colorByCfg}
              onChange={e => setColorByCfg(e.target.checked)}></input>{' '}
            Color by configuration
          </label>
          <label className="checkbox ml-2 mt-4">
            <input
              type="checkbox"
              checked={showLabels}
              onChange={e => setShowLabels(e.target.checked)}></input>{' '}
            Show labels
          </label>
        </div>
        <article className="message cell">
          <div className="message-body">
            Number of nodes: {data.length} <br />
            Number of edges: {edges.length}
          </div>
        </article>
      </div>

      <GraphViz2
        nodes={data}
        colorByCfg={colorByCfg}
        showLabels={showLabels}
        links={edges}
        openTarget={(name: string) => {
          const url = new URL(window.location.href)
          url.searchParams.set('target', name)
          url.searchParams.delete('graph')
          window.open(url.toString(), '_blank')
        }}
      />
    </>
  )
}
