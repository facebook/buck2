/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useState} from 'react'
import {Build} from '../fbs/output-format'
import {Node} from './GraphView2'
import {GraphViz2} from './GraphViz2'
import {LinkObject, NodeObject} from 'react-force-graph-2d'

enum DisplayType {
  rootNode,
  passesFilters,
  changedFiles,
  hidden,
  highlighted,
  actionsRan,
  // For this node, 1st order deps will be shown
  expanded,
  // Node that is there because the parent was expanded via click
  clickDep,
}

const displayTypeColors: {[key in DisplayType]: string} = {
  // https://coolors.co/1c77c3-39a9db-9ec1a3-cfe0c3-e9724c
  [DisplayType.rootNode]: '#1a181b',
  [DisplayType.passesFilters]: '#1c77c3',
  [DisplayType.changedFiles]: '#00C49A',
  [DisplayType.highlighted]: '#e9724c',
  [DisplayType.expanded]: 'red',
  [DisplayType.clickDep]: 'yellow',
  [DisplayType.actionsRan]: '#9C528B',
  [DisplayType.hidden]: 'gray', // doesn't matter
}

// More than this and graph starts to feel sluggish
const MAX_NODES = 500

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
  allTargets: Map<string, number>
}) {
  const {nodes, build} = props

  const nodeMap: Map<number, DisplayNode> = new Map()
  for (const [k, node] of nodes) {
    nodeMap.set(k, {...node, allowedDeps: new Map(), displayType: DisplayType.hidden})
  }

  const [colorByCfg, setColorByCfg] = useState(false)
  const [showLabels, setShowLabels] = useState(true)
  const [transitiveReduction, setTransitiveReduction] = useState(true)
  const [includeContaining, setIncludeContaining] = useState<string>('')
  const [excludeContaining, setExcludeContaining] = useState<string>('')
  const [highlighted, setHighlighted] = useState<string | null>(null)
  const [expandedNodes, setExpandedNodes] = useState<Set<number>>(new Set())

  let nodeCounter = 0
  const includeRegex = new RegExp(includeContaining)
  const excludeRegex = new RegExp(excludeContaining)

  // Apply filters
  for (const [k, node] of nodeMap) {
    const target = build.targets(k)!
    const label = target.label()!

    // Add highlighted
    if (highlighted) {
      if (label.includes(highlighted)) {
        node.displayType = DisplayType.highlighted
      }
    }

    // Including means we can hide everything that doesn't match the filter
    if (includeContaining.length > 1 && includeRegex.test(label)) {
      node.displayType = DisplayType.passesFilters
    }

    // Excluding can hide everything except root node
    if (excludeContaining.length > 1 && excludeRegex.test(label)) {
      node.displayType = DisplayType.hidden
    }

    // Click deps
    if (expandedNodes.has(k)) {
      node.displayType = DisplayType.expanded
    }
    for (const r of node.rdeps) {
      if (expandedNodes.has(r)) {
        if (node.displayType === DisplayType.hidden) {
          node.displayType = DisplayType.clickDep
        }
      }
    }

    // Prevent graph from having too many nodes and slowing everything down
    if (node.displayType != DisplayType.hidden) {
      nodeCounter += 1
    }
    if (nodeCounter > MAX_NODES) {
      node.displayType = DisplayType.hidden
    }
  }

  // Always set root node
  nodeMap.get(0)!.displayType = DisplayType.rootNode

  let filteredNodes = new Map<number, DisplayNode>()
  for (const [k, node] of nodeMap) {
    if (showNode(node)) {
      filteredNodes.set(k, node)
    }
  }

  let displayNodes: Map<number, DisplayNode> = new Map()
  for (const [k, node] of nodeMap) {
    if (showNode(node)) {
      displayNodes.set(k, node)
    }
  }

  // For each node A that goes, traverse the graph bottom up BFS
  // until another node that goes is found, then add node A as allowedDep
  // Also stores shortest path length from last allowed to later add as edge label

  for (const [k, _] of displayNodes) {
    let visited: Map<number, number> = new Map()
    visited.set(k, 0)
    let stack = [k]

    while (stack.length > 0) {
      const n1 = stack.shift()!

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

  if (transitiveReduction) {
    for (const [k, node] of displayNodes) {
      let queue = [...node.allowedDeps.keys()]
      let visited = new Set()
      // for each dep we check all its transitive deps
      while (queue.length != 0) {
        const curr = queue.shift()!
        if (visited.has(curr)) {
          continue
        } else {
          visited.add(curr)
          let n = displayNodes.get(curr)!
          for (const [k, _] of n.allowedDeps) {
            // reachable, delete link from node in question
            node.allowedDeps.delete(k)
            queue.push(k)
          }
        }
      }
    }
  }

  // Build graph in a format that the graph library understands
  const data: NodeObject[] = []
  const edges: LinkObject[] = []

  for (const [k, node] of displayNodes) {
    const target = build.targets(k)!

    // Add nodes to graph
    data.push({
      val: 0.5,
      id: k,
      name: target.label()!,
      color: colorByCfg ? undefined : displayTypeColors[node.displayType],
      cfg: target.label()!.split(' ')[1],
    })
  }

  for (const [k, node] of displayNodes) {
    // Add edges
    for (const [d, counter] of node.allowedDeps) {
      if (!displayNodes.has(d)) {
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
    setIncludeContaining(inputValue('includeContaining'))
    setExcludeContaining(inputValue('excludeContaining'))

    // Highlight by label
    setHighlighted(inputValue('highlightNode'))
  }

  function toggleNodeExpand(n: number) {
    let newExpanded = new Set(expandedNodes)
    if (newExpanded.has(n)) {
      newExpanded.delete(n)
    } else {
      newExpanded.add(n)
    }
    setExpandedNodes(newExpanded)
  }

  return (
    <>
      <div className="grid mt-4">
        <article className="message cell is-primary">
          <div className="message-header">
            <p>Build stats</p>
          </div>
          <div className="message-body">
            Nodes shown: {data.length} <br />
            Edges shown: {edges.length} <br />
          </div>
        </article>
        <div className="cell">
          <div className="field">
            <label className="label is-size-7">Filter by labels:</label>
            <div className="control">
              <input
                id="includeContaining"
                className="input is-small"
                type="text"
                placeholder={'Include matching regex'}
              />
            </div>
            <div className="control">
              <input
                id="excludeContaining"
                className="input is-small"
                type="text"
                placeholder={'Exclude matching regex'}
              />
            </div>
          </div>
        </div>
        <div className="cell">
          <div className="field">
            <label className="label is-size-7">Highlight target by label:</label>
            <div className="control">
              <input
                id="highlightNode"
                className="input is-small"
                type="text"
                placeholder="Label to highlight"
              />
            </div>
          </div>
          <div className="cell">
            <button type="submit" onClick={applyFilters} className="button is-dark is-small">
              <span>Apply filters</span>
            </button>
          </div>
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
          <label className="checkbox ml-2 mt-4">
            <input
              type="checkbox"
              checked={transitiveReduction}
              onChange={e => setTransitiveReduction(e.target.checked)}></input>{' '}
            Transitive reduction
          </label>
        </div>
      </div>
      <h3 className="title is-3">Showing targets with executed actions</h3>
      <GraphViz2
        toggleNodeExpand={toggleNodeExpand}
        nodes={data}
        colorByCfg={colorByCfg}
        showLabels={showLabels}
        links={edges}
      />
    </>
  )
}
