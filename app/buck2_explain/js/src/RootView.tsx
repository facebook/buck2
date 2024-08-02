/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useContext} from 'react'
import {Target} from './Target'
import {DataContext} from './App'
import {SearchBox} from './SearchBox'

/**
 * Shows the root target
 */
export function RootView(props: {view: string}) {
  const {rootTarget} = useContext(DataContext)

  return (
    <>
      <SearchBox />
      {rootTarget == null ? <p>No root target</p> : <Target target={rootTarget} />}
    </>
  )
}
