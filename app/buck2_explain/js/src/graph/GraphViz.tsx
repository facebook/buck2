/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useRef} from 'react'
import ForceGraph2D, {LinkObject, NodeObject} from 'react-force-graph-2d'

export function GraphViz(props: {
  nodes: NodeObject[]
  links: LinkObject[]
  setPath: (name: string) => void
  openTarget: (name: string) => void
}) {
  const {nodes, links, setPath, openTarget} = props
  const graphRef = useRef<any>(null)
  const dagMode = links.length / nodes.length > 3 ? 'td' : undefined

  return (
    <ForceGraph2D
      ref={graphRef}
      graphData={{nodes, links}}
      onNodeClick={(node, _event) => {
        setPath(node.name)
      }}
      onNodeRightClick={(node, _event) => {
        openTarget(node.name)
      }}
      // cooldown + warmup ticks make the graph render already in its final form
      cooldownTicks={1}
      enableNodeDrag={true}
      warmupTicks={100}
      // looks
      linkDirectionalArrowLength={10 / Math.pow(nodes.length, 0.2)}
      linkDirectionalArrowRelPos={1}
      linkCurvature={0.2}
      linkWidth={3 / Math.pow(nodes.length, 0.5)}
      linkHoverPrecision={6}
      dagMode={dagMode}
      nodeAutoColorBy="group"
    />
  )
}
