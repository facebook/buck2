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
 * Header that goes on every view
 */
export function Header() {
  const {rootTarget} = useContext(DataContext)

  if (!rootTarget) {
    return null
  }

  return (
    <div className="navbar" role="navigation" aria-label="main navigation">
      <div className="navbar-brand">
        <Link className="bold no-underline navbar-item" to={{}}>
          <span className="icon mr-1">
            <i className="fa fa-bullseye" />
          </span>
          {rootTarget.configuredTargetLabel()}
        </Link>
      </div>
      <div className="navbar-menu">
        <div className="navbar-start">
          <Link className="bold no-underline navbar-item" to={{graph: ''}}>
            <span className="icon mr-1">
              <i className="fa fa-project-diagram" />
            </span>
            Graph
          </Link>
        </div>
      </div>
    </div>
  )
}
