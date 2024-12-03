/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useContext} from 'react'
import {DataContext} from '../App'
import {GraphImpl2} from './GraphImpl2'
import {QueryKey} from '../Router'
import {formatTargetLabel} from '../formatTargetLabel'

export interface Node {
  value: number
  deps: number[]
  rdeps: number[]
}

type CategoryOption = {category: string; count: number; checked: false}

function defaultNode(): Node {
  return {
    value: 0,
    deps: [],
    rdeps: [],
  }
}

export function GraphView2(props: {view: QueryKey}) {
  const {build, allTargets} = useContext(DataContext)
  if (build == null) {
    // TODO: this should show a loading sign
    return null
  }

  // Build better data structure
  let nodeMap = new Map<number, Node>()

  // Create nodes
  for (let i = 0; i < build.targetsLength(); i++) {
    if (nodeMap.get(i) == null) {
      nodeMap.set(i, {
        ...defaultNode(),
        value: i,
      })
    }
  }

  // Record deps and rdeps
  for (const [k, node] of nodeMap) {
    const target = build.targets(k)!

    for (let i = 0; i < target.depsLength(); i++) {
      const d = allTargets[formatTargetLabel(target.deps(i)!)]

      // Deps
      node.deps.push(d)

      // Rdeps
      if (d === k) {
        throw Error('wth')
      }
      nodeMap.get(d)!.rdeps.push(k)
    }
  }

  // Nodes with file changes
  const containsChangedFile = []
  for (const [k, _node] of nodeMap) {
    const target = build.targets(k)!
    if (target.changedFilesLength() > 0) {
      containsChangedFile.push(k)
    }
  }

  if (containsChangedFile.length === 0) {
    return <p>No file changes registered</p>
  }

  return (
    <div className="mx-4">
      <GraphImpl2 nodes={nodeMap} build={build} allTargets={allTargets} />
    </div>
  )
}
