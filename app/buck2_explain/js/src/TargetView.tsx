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
import {RootSpan} from './RootSpan'

export function TargetView(props: {view: string}) {
  const {allTargets, build, rootTarget} = useContext(DataContext)

  const params = new URLSearchParams(window.location.search)
  const targetLabel = params.get(props.view) ?? null
  const target = targetLabel == null ? null : build?.targets(allTargets[targetLabel])

  let targetElement
  if (target == null) {
    targetElement = <p>No target found</p>
  } else {
    targetElement = <Target target={target} />
  }

  return (
    <>
      {rootTarget ? <RootSpan /> : null}
      {targetElement}
    </>
  )
}
