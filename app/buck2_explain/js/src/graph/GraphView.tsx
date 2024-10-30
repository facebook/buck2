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

export function GraphView(props: {view: QueryKey}) {
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
      node.rdeps.push(k)
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
        nodes={nodeMap}
        build={build}
        categoryOptions={categoryOptions}
        allTargets={allTargets}
      />
    </div>
  )
}
