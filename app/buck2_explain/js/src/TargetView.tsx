/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

import React, {useContext} from 'react'
import {DataContext} from './App'
import {Target} from './Target'
import {QueryKey, RouterContext} from './Router'

export function TargetView(props: {view: QueryKey}) {
  const {allTargets, build} = useContext(DataContext)
  const {params} = useContext(RouterContext)

  const urlParams = new URLSearchParams(params)
  const targetLabel = urlParams.get(props.view) ?? null
  if (build == null) {
    return <p>Loading...</p>
  }
  const target = targetLabel == null ? null : build.targets(allTargets.get(targetLabel)!)

  const tab = urlParams.get(QueryKey.TargetTab)

  const view = target == null ? <p>No target found</p> : <Target target={target} tab={tab} />
  return <div className="mx-4">{view}</div>
}
