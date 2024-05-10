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
import {RootSpan} from './RootSpan'
import {Link} from './Router'

export function SearchView(props: {view: string}) {
  const {allTargets, rootTarget} = useContext(DataContext)

  const params = new URLSearchParams(window.location.search)
  const search = params.get(props.view)

  if (search == null || search.length < 3) {
    return (
      <>
        {rootTarget ? <RootSpan /> : null}
        <p>Invalid search "{search}", try again</p>
      </>
    )
  }

  const res: string[] = []
  for (let k of Object.keys(allTargets)) {
    if (k.includes(search)) {
      res.push(k)
    }
  }

  return (
    <>
      {rootTarget ? <RootSpan /> : null}
      <h2>Showing targets labels containing "{search}"</h2>
      <ul>
        {res.map(label => (
          <li key={label}>
            <Link to={{target: label}}>{label}</Link>
          </li>
        ))}
      </ul>
    </>
  )
}
