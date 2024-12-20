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

export function GraphView2(props: {view: QueryKey}) {
  const {build, allTargets, graph} = useContext(DataContext)
  if (build == null) {
    // TODO: this should show a loading sign
    return null
  }

  // Stats
  let totalFileChanges = 0
  let totalActions = 0
  for (const [k, _node] of graph) {
    const target = build.targets(k)!
    totalFileChanges += target.changedFilesLength()
    // TODO iguridi: make it match whatran
    totalActions += target.actionsLength()
  }

  if (totalActions === 0 && totalFileChanges === 0) {
    return <p>No actions or file changes registered</p>
  }

  return (
    <div className="mx-4">
      <GraphImpl2
        nodes={graph}
        build={build}
        allTargets={allTargets}
        totalActions={totalActions}
        totalFileChanges={totalFileChanges}
      />
    </div>
  )
}
