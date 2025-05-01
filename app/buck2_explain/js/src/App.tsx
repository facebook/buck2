/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Add css to the bundle
import './app.css'

import React, {useEffect, useState} from 'react'
import {createRoot} from 'react-dom/client'

import {ByteBuffer} from 'flatbuffers'
import {Build, ConfiguredTargetNode} from './fbs/explain'
import {QueryKey, Router} from './Router'
import {TargetView} from './TargetView'
import {SearchView} from './SearchView'
import {GraphView2} from './graph2/GraphView2'
import {Navbar} from './Navbar'
import {formatTargetLabel} from './formatTargetLabel'

const INITIAL_STATE = {
  build: null,
  rootTarget: null,
  graph: new Map<number, Node>(),
  allTargets: new Map<string, number>(),
}

export interface Node {
  value: number
  deps: number[]
  rdeps: number[]
}

type STATE_TYPE = {
  build: Build | null
  rootTarget: ConfiguredTargetNode | null
  graph: Map<number, Node>
  allTargets: Map<string, number>
}

function defaultNode(): Node {
  return {
    value: 0,
    deps: [],
    rdeps: [],
  }
}

export const DataContext = React.createContext<STATE_TYPE>(INITIAL_STATE)

function App() {
  const [data, setData] = useState<STATE_TYPE>(INITIAL_STATE)

  /**
   * Loads initial information on page load
   */
  useEffect(() => {
    const fetchData = async () => {
      // keep this line as is, it will be replaced later
      let blobBase64 = 'XXDATAXX'
      try {
        blobBase64 = (await import('./data')).DATA
      } catch (error) {
        console.info('./data.ts not found, using replaced data')
      }

      const decodedString = atob(blobBase64)
      // TODO iguridi: decode blob better
      const byteArray = new Uint8Array(decodedString.length)
      for (let i = 0; i < decodedString.length; i++) {
        byteArray[i] = decodedString.charCodeAt(i)
      }

      const buf = new ByteBuffer(byteArray)

      // Get an accessor to the root object inside the buffer.
      const build = Build.getRootAsBuild(buf)
      // TODO iguridi: just show 1 target for now
      const rootTarget = build.targets(0)

      const allTargets: Map<string, number> = new Map()
      for (let i = 0; i < build.targetsLength(); i++) {
        let target = build.targets(i)!
        // Unique identifier for target
        let label = formatTargetLabel(target.label()!)
        allTargets.set(label, i)
      }

      // Build better data structure
      let graph = new Map<number, Node>()

      // Create nodes
      for (let i = 0; i < build.targetsLength(); i++) {
        if (graph.get(i) == null) {
          graph.set(i, {
            ...defaultNode(),
            value: i,
          })
        }
      }

      // Record deps and rdeps
      for (const [k, node] of graph) {
        const target = build.targets(k)!

        for (let i = 0; i < target.depsLength(); i++) {
          const d = allTargets.get(formatTargetLabel(target.deps(i)!))!

          // Deps
          node.deps.push(d)

          // Rdeps
          if (d === k) {
            throw Error('Found dependency on self')
          }
          graph.get(d)!.rdeps.push(k)
        }
      }

      // TODO iguridi: filter this out in rust side of things
      // Only show nodes that rdeps of node with file changes or actions
      const containsActionOrChangedFile: Set<number> = new Set()
      for (let i = 0; i < build.targetsLength(); i++) {
        const target = build.targets(i)!
        if (target.changedFilesLength() > 0 || target.actionsLength() > 0) {
          containsActionOrChangedFile.add(i)
        }
      }

      let visited = new Set()
      let weWantThis = new Set()
      let stack = [...containsActionOrChangedFile]
      while (stack.length > 0) {
        const k = stack.shift()!
        visited.add(k)
        weWantThis.add(k)
        const node = graph.get(k)!
        for (const r of node.rdeps) {
          if (!visited.has(r)) {
            stack.push(r)
          }
        }
      }
      const filteredNodes: Map<number, Node> = new Map(
        graph.entries().filter(([k, _node]) => weWantThis.has(k)),
      )

      // This should run just once total
      setData({build, allTargets, rootTarget, graph: filteredNodes})
    }
    fetchData()
  }, [])

  return (
    <DataContext.Provider value={data}>
      <Router>
        <Navbar />
        <TargetView view={QueryKey.TargetView} />
        <SearchView view={QueryKey.SearchView} />
        <GraphView2 view={QueryKey.RootView} />
      </Router>
    </DataContext.Provider>
  )
}

const container = document.getElementById('root') as HTMLElement
const root = createRoot(container)

root.render(<App />)
