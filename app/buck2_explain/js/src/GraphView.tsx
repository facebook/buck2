/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useContext, useRef} from 'react'
import {Target} from './Target'
import {DataContext} from './App'
import ForceGraph2D, {LinkObject, NodeObject} from 'react-force-graph-2d'

type Node = {
  value: number
  deps: number[]
}

function defaultNode(): Node {
  return {
    value: 0,
    deps: [],
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
  for (let i = 0; i < 200; i++) {
    const target = build.targets(i)!

    // Create node object
    if (nodeMap.get(i) == null) {
      nodeMap.set(i, {
        ...defaultNode(),
        value: i,
      })
    }

    // Set deps
    for (let d = 0; d < target.depsLength(); d++) {
      const dep = target.deps(d)!
      const j = allTargets[dep]
      if (nodeMap.has(j)) {
        nodeMap.get(i)!.deps.push(j)
      }
    }
  }

  const data: NodeObject[] = []
  const edges: LinkObject[] = []

  for (const [k, _] of nodeMap) {
    const target = build.targets(k)!

    // Add nodes to graph
    let options = {}

    data.push({
      ...options,
      id: k,
      name: target.configuredTargetLabel()!,
    })
  }

  for (const [k, node] of nodeMap) {
    // Add edges
    for (const d of node.deps) {
      if (nodeMap.has(d)) {
        edges.push({
          source: k,
          target: d,
        })
      }
    }
  }

  const graphRef = useRef<any>(null)

  return (
    <ForceGraph2D
      ref={graphRef}
      graphData={{nodes: data, links: edges}}
      onNodeClick={(node, _event) => console.log(node.nodeVal, 'click!')}
      linkDirectionalArrowLength={1}
      enableNodeDrag={false}
      linkDirectionalArrowRelPos={1}
      linkCurvature={0.1}
      onEngineTick={() => {
        if (Math.random() < 0.3) graphRef?.current?.zoomToFit()
      }}
      cooldownTime={2000}
    />
  )
}
