/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useEffect, useRef, useState} from 'react'
import ForceGraph2D, {LinkObject, NodeObject, ForceGraphProps} from 'react-force-graph-2d'

export function GraphViz2(props: {
  nodes: NodeObject[]
  links: LinkObject[]
  colorByCfg: boolean
  showLabels: boolean
  toggleNodeExpand: (name: number) => void
}) {
  const {nodes, links, showLabels, toggleNodeExpand} = props
  const graphRef = useRef<any>(null)

  const [firstLoad, setFirstLoad] = useState(true)

  // Show labels optionally
  let paintLabels: ForceGraphProps['nodeCanvasObject'] = undefined
  let paintMode: ForceGraphProps['nodeCanvasObjectMode'] = undefined
  if (showLabels) {
    paintLabels = (node, ctx, _globalScale) => {
      const label = node.name.split(' ')[0].split(':')[1]
      const fontSize = 2
      ctx.font = `${fontSize}px Sans-Serif`
      const textWidth = ctx.measureText(label).width
      const padding = fontSize * 0.1
      const bckgDimensions = [textWidth + padding, fontSize + padding] // some padding
      const angle = -Math.PI / 10 // 18 degrees
      ctx.fillStyle = 'rgba(255, 255, 255, 0.9)'

      // Need to save before rotating and/or translating
      ctx.save()
      ctx.translate(node.x!, node.y!)
      ctx.rotate(angle)

      ctx.fillRect(0, 0 - bckgDimensions[1] / 2, bckgDimensions[0], bckgDimensions[1])

      ctx.textAlign = 'left'
      ctx.textBaseline = 'middle'
      ctx.fillStyle = '#000'
      ctx.fillText(label, 0 + padding, 0)
      // Reset translation and rotation back
      ctx.restore()
    }
    paintMode = _node => 'after'
  }

  return (
    <ForceGraph2D
      ref={graphRef}
      width={Math.max(document.documentElement.clientWidth, 1000)}
      graphData={{nodes, links}}
      nodeCanvasObjectMode={paintMode}
      nodeCanvasObject={paintLabels}
      onNodeClick={(node, _event) => {
        props.toggleNodeExpand(node.id! as number)
      }}
      // cooldown + warmup ticks make the graph render already in its final form
      cooldownTicks={1}
      enableNodeDrag={true}
      warmupTicks={100}
      // looks
      nodeAutoColorBy={props.colorByCfg ? 'cfg' : undefined}
      linkDirectionalArrowLength={3 / Math.pow(nodes.length, 0.2)}
      linkDirectionalArrowRelPos={1}
      linkCurvature={0.2}
      linkWidth={10 / Math.pow(links.length, 0.5)}
      linkHoverPrecision={6}
      dagMode="td"
      onEngineStop={() => {
        // zoom to fit window, but only the first time the graph loads
        if (firstLoad) {
          graphRef.current.zoomToFit(0, 200)
          setFirstLoad(false)
        }
      }}
    />
  )
}
