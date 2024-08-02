/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useContext} from 'react'
import {DataContext} from './App'
import {Target} from './Target'
import {RouterContext, TARGET_TAB} from './Router'
import {SearchBox} from './SearchBox'

export function TargetView(props: {view: string}) {
  const {allTargets, build} = useContext(DataContext)
  const {params} = useContext(RouterContext)

  const urlParams = new URLSearchParams(params)
  const targetLabel = urlParams.get(props.view) ?? null
  const target = targetLabel == null ? null : build?.targets(allTargets[targetLabel])

  const tab = urlParams.get(TARGET_TAB)

  return (
    <>
      <SearchBox />
      {target == null ? <p>No target found</p> : <Target target={target} tab={tab} />}
    </>
  )
}
