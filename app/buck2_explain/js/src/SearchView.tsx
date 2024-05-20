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
import {Link, RouterContext, TARGET_VIEW} from './Router'

export function SearchView(props: {view: string}) {
  const {allTargets} = useContext(DataContext)
  const {params} = useContext(RouterContext)

  const urlParams = new URLSearchParams(params)
  const search = urlParams.get(props.view)

  if (search == null || search.length < 3) {
    return <p>Invalid search "{search}", try again</p>
  }

  const res: string[] = []
  for (let k of Object.keys(allTargets)) {
    if (k.includes(search)) {
      res.push(k)
    }
  }

  return (
    <>
      <h2>Showing targets labels containing "{search}"</h2>
      <ul>
        {res.map(label => (
          <li key={label}>
            <Link to={new Map().set(TARGET_VIEW, label)}>{label}</Link>
          </li>
        ))}
      </ul>
    </>
  )
}
