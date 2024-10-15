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
import {Node} from './GraphView'
import {GraphViz} from './GraphViz'
import {LinkObject, NodeObject} from 'react-force-graph-2d'
import {longestPathTree, shortestPathTree} from './graph'

enum NodeSizeOption {
  transitiveDeps,
  transitiveSrcs,
}

enum ShowPaths {
  Shortest,
  Longest,
  All,
}

enum DisplayType {
  rootNode,
  passesFilters,
  somepath,
  hidden,
  highlighted,
}

const displayTypeColors: {[key in DisplayType]: string} = {
  // https://coolors.co/1c77c3-39a9db-9ec1a3-cfe0c3-e9724c
  [DisplayType.rootNode]: '#1a181b',
  [DisplayType.passesFilters]: '#1c77c3',
  [DisplayType.somepath]: '#9EC1A3',
  [DisplayType.highlighted]: '#e9724c',
  [DisplayType.hidden]: 'gray', // doesn't matter
}

interface DisplayNode extends Node {
  allowedDeps: Map<number, number>
  displayType: DisplayType
}

function showNode(node: DisplayNode) {
  return node.displayType != DisplayType.hidden
}

type DepsGraph = Map<number, {deps: number[]; rdeps: number[]}>

function toLeanGraph(graph: DepsGraph): Map<number, number[]> {
  let newGraph = new Map()
  for (const [k, node] of graph) {
    newGraph.set(k, node.deps)
  }
  return newGraph
}

function fromLeanGraph(graph: Map<number, number[]>): DepsGraph {
  let newGraph = new Map()
  for (const [k, deps] of graph) {
    newGraph.set(k, {deps, rdeps: []})
  }
  for (const [k, deps] of graph) {
    for (const d of deps) {
      newGraph.get(d)!.rdeps.push(k)
    }
  }
  return newGraph
}

// Here it goes everything that has to recompute on user interaction.
// On big graphs recomputing less matters
export function GraphImpl(props: {
  nodes: Map<number, Node>
  build: Build
  graphDeps: DepsGraph
  maxSrcs: number
  allTargets: {[key: string]: number}
  categoryOptions: {category: string; count: number; checked: boolean}[]
}) {
  const {nodes, build, categoryOptions, allTargets, maxSrcs} = props

  const nodeMap: Map<number, DisplayNode> = new Map()
  for (const [k, node] of nodes) {
    nodeMap.set(k, {...node, allowedDeps: new Map(), displayType: DisplayType.hidden})
  }

  const [categories, setCategories] = useState(categoryOptions)
  const [colorByCfg, setColorByCfg] = useState(false)
  const [showLabels, setShowLabels] = useState(false)
  const [includeContaining, setIncludeContaining] = useState<string[]>([])
  const [excludeContaining, setExcludeContaining] = useState<string[]>([])
  const [somepath, setSomepath] = useState<Set<number>>(new Set())
  const [highlighted, setHighlighted] = useState<string | null>(null)
  const [showPaths, setShowPaths] = useState(ShowPaths.All)
  const [selectedOption, setSelectedOption] = useState(NodeSizeOption.transitiveDeps)

  // Choose which edges to show
  const chooseEdges = (graph: DepsGraph, show: ShowPaths) => {
    let newGraph
    const lean = toLeanGraph(graph)
    if (show === ShowPaths.Shortest) {
      newGraph = shortestPathTree(lean, 0)
    } else if (show === ShowPaths.Longest) {
      newGraph = longestPathTree(lean, 0)
    } else {
      newGraph = lean // ShowPaths.All
    }
    return fromLeanGraph(newGraph)
  }
  const graphDeps = chooseEdges(props.graphDeps, showPaths)

  const activeCategories = categories.filter(v => v.checked).map(v => v.category)

  if (somepath.size > 0) {
    for (const k of somepath) {
      nodeMap.get(k)!.displayType = DisplayType.somepath
    }
  } else {
    // Intersection of 'includes', minus 'excludes'
    for (const [k, node] of nodeMap) {
      const target = build.targets(k)!
      const label = target.configuredTargetLabel()!

      // When null, means it wasn't affected by any of the filters and to use default
      let passesFilters = null

      // Filter by category
      if (activeCategories.length > 0) {
        passesFilters = activeCategories.includes(target.type()!)
      }

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
    }

    // Always set root node
    nodeMap.get(0)!.displayType = DisplayType.rootNode
  }

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

      for (const r of graphDeps.get(n1)!.rdeps) {
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

    const [sizeValue, maxValue] =
      selectedOption === NodeSizeOption.transitiveDeps
        ? [nodeMap.get(k)!.transitiveDeps, nodeMap.size]
        : selectedOption === NodeSizeOption.transitiveSrcs
        ? [nodeMap.get(k)!.transitiveSrcs, maxSrcs - 1]
        : [1, 1] // This should never happen

    // Add nodes to graph
    data.push({
      val: translateValues(sizeValue, maxValue) + 0.5, // controls size
      id: k,
      name: target.configuredTargetLabel()!,
      color: colorByCfg ? undefined : displayTypeColors[node.displayType],
      cfg: target.configuredTargetLabel()!.split('#')[1],
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
    // TODO iguridi: this is nasty, but should do for now
    setSomepath(new Set())

    const inputValue = (id: string) =>
      (document.getElementById(id) as HTMLInputElement).value.trim()

    // Include exclude by label
    const inc = inputValue('includeContaining')
    setIncludeContaining(inc ? inc.split(',') : [])
    const exc = inputValue('excludeContaining')
    setExcludeContaining(exc ? exc.split(',') : [])

    // Highlight by label
    setHighlighted(inputValue('highlightNode'))

    // Include by rule type
    const checkboxes = document.querySelectorAll('#checkboxes input[type="checkbox"]')
    for (let i = 0; i < checkboxes.length; i++) {
      categories[i].checked = checkboxes[i].checked
    }
    setCategories([...categories])
  }

  function findPath() {
    const inputValue = (id: string) =>
      (document.getElementById(id) as HTMLInputElement).value.trim()

    // Include by path
    const pathFrom = inputValue('pathFrom')
    const pathTo = inputValue('pathTo')

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
        for (let d of graphDeps.get(node)!.deps) {
          if (!parentOf.has(d)) {
            parentOf.set(d, node)
            queue.push(d)
          }
        }
      }

      // set allowed if in path
      let path = new Set<number>()
      let node = to
      while (node) {
        path.add(node)
        node = parentOf.get(node)
      }

      setSomepath(path)
    }
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
          <div id="checkboxes">
            <div className="field">
              <label className="label">Include targets with rule types:</label>
              <RuleTypeDropdown options={categories} activeCount={activeCategories.length} />
            </div>
          </div>
          <button type="submit" onClick={applyFilters} className="button is-dark">
            <span>Apply filters</span>
          </button>
        </div>
        <div className="cell">
          <div className="select">
            <select
              value={selectedOption}
              onChange={e => setSelectedOption(parseInt(e.target.value))}>
              <option value={NodeSizeOption.transitiveDeps}>Size by transitive deps count</option>
              <option value={NodeSizeOption.transitiveSrcs}>Size by transitive srcs count</option>
            </select>
          </div>
          <div className="select">
            <select value={showPaths} onChange={e => setShowPaths(parseInt(e.target.value))}>
              <option value={ShowPaths.All}>Show all edges</option>
              <option value={ShowPaths.Shortest}>Show shortest path from root</option>
              <option value={ShowPaths.Longest}>Show longest path from root</option>
            </select>
          </div>
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
          <div className="cell">
            <button type="submit" onClick={findPath} className="button is-dark">
              <span>Find some path</span>
            </button>
          </div>
        </div>
        <article className="message cell">
          <div className="message-body">
            Number of nodes: {data.length} <br />
            Number of edges: {edges.length}
          </div>
        </article>
      </div>

      <GraphViz
        nodes={data}
        colorByCfg={colorByCfg}
        showLabels={showLabels}
        links={edges}
        setPath={(name: string) => {
          const fromInput = document.getElementById('pathFrom') as HTMLInputElement
          const toInput = document.getElementById('pathTo') as HTMLInputElement
          if (!fromInput.value) {
            fromInput.value = name
          } else if (!toInput.value) {
            toInput.value = name
          } else {
            fromInput.value = name
            toInput.value = ''
          }
        }}
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

function translateValues(inputValue: number, maxValue: number) {
  const outputMin = 0.01
  const outputMax = 4
  const normalized = inputValue / maxValue
  const scaled = normalized * outputMax + outputMin
  return scaled
}
