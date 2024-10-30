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
import {GraphImpl} from './GraphImpl'
import {QueryKey} from '../Router'
import {formatTargetLabel} from '../formatTargetLabel'

export interface Node {
  value: number
  transitiveDeps: number
  transitiveSrcs: number
  srcs: number
}

type CategoryOption = {category: string; count: number; checked: false}

function defaultNode(): Node {
  return {
    value: 0,
    transitiveDeps: 0,
    transitiveSrcs: 0,
    srcs: 0,
  }
}

export function GraphView(props: {view: QueryKey}) {
  const {build, allTargets} = useContext(DataContext)
  if (build == null) {
    // TODO: this should show a loading sign
    return null
  }

  // Build better data structure
  let nodeMap = new Map<number, Node>()
  let graphDeps: Map<number, {deps: number[]; rdeps: number[]}> = new Map()

  // Create nodes
  for (let i = 0; i < build.targetsLength(); i++) {
    if (nodeMap.get(i) == null) {
      nodeMap.set(i, {
        ...defaultNode(),
        value: i,
      })
      graphDeps.set(i, {deps: [], rdeps: []})
    }
  }

  // Record deps, rdeps and srcs
  let maxSrcs = 0
  for (const [k, node] of nodeMap) {
    const target = build.targets(k)!

    // Srcs
    const srcs = Number(target.srcs()) // TODO iguridi: long type for srcs is not needed
    maxSrcs += srcs
    node.srcs = srcs

    for (let i = 0; i < target.depsLength(); i++) {
      const d = allTargets[formatTargetLabel(target.deps(i)!)]

      // Deps
      graphDeps.get(k)!.deps.push(d)

      // Rdeps
      if (d === k) {
        throw Error('wth')
      }
      graphDeps.get(d)!.rdeps.push(k)
    }
  }

  // Sum transitive deps and srcs. For each node, we traverse all the transitive rdeps
  for (const [k, node] of nodeMap) {
    let visited = new Set()
    let rdeps = new Set(graphDeps.get(k)!.rdeps)

    while (rdeps.size > 0) {
      let next: Set<number> = new Set()
      for (const r of rdeps) {
        if (!visited.has(r)) {
          // Add transitive deps
          const rnode = nodeMap.get(r)!
          const rdeps2 = graphDeps.get(r)!.rdeps
          rnode.transitiveDeps += 1
          // Add transitive srcs
          rnode.transitiveSrcs += node.srcs

          visited.add(r)
          for (const r2 of rdeps2) {
            next.add(r2)
          }
        }
      }
      rdeps = next
    }
  }

  const extractCategories = (): CategoryOption[] => {
    const categoriesCounter = new Map()
    for (const [k, _node] of nodeMap) {
      const target = build.targets(k)
      const type = target!.type()!
      categoriesCounter.set(type, (categoriesCounter.get(type) ?? 0) + 1)
    }

    let categoryOptions: CategoryOption[] = []
    for (const [category, count] of categoriesCounter) {
      categoryOptions.push({category, count, checked: false})
    }
    categoryOptions.sort((a, b) => {
      if (a.category < b.category) {
        return -1
      } else {
        return 1
      }
    })

    return categoryOptions
  }
  const categoryOptions = extractCategories()

  return (
    <div className="mx-4">
      <GraphImpl
        graphDeps={graphDeps}
        nodes={nodeMap}
        build={build}
        categoryOptions={categoryOptions}
        allTargets={allTargets}
        maxSrcs={maxSrcs}
      />
    </div>
  )
}
