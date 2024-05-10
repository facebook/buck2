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
import {Link} from './Router'

/**
 * Acts as a normal webpage banner, where clicking on it sends the user back to the root page of the website
 */
export function RootSpan() {
  const {rootTarget} = useContext(DataContext)

  return (
    <Link to={new Map()}>
      <i>
        <span>{rootTarget?.configuredTargetLabel()}</span>
      </i>
    </Link>
  )
}
