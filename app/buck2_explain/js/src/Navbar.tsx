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
import {Link} from './Router'
import {SearchBox} from './SearchBox'

/**
 * Header that goes on every view
 */
export function Navbar() {
  const {rootTarget} = useContext(DataContext)

  if (!rootTarget) {
    return null
  }

  return (
    <nav className="navbar" role="navigation" aria-label="main navigation">
      <div className="navbar-brand">
        <Link className="bold has-text-info-bold no-underline navbar-item" to={{}}>
          <span className="icon mr-1">
            <i className="fa fa-bullseye" />
          </span>
          {rootTarget.label()!.targetLabel()}
        </Link>
      </div>
      <div className="navbar-menu">
        <div className="navbar-end">
          <div className="navbar-item has-text-info-bold">
            <SearchBox />
          </div>
        </div>
      </div>
    </nav>
  )
}
