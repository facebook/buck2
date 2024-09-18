/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useContext, useState} from 'react'
import {DataContext} from './App'
import {Link, QueryKey, RouterContext} from './Router'
import {indexCache, indexEverything} from './flexSearch'
import {SearchBox} from './SearchBox'

function Checkbox(props: {
  checked: boolean
  onChange: (event: {target: {checked: boolean | ((prevState: boolean) => boolean)}}) => void
}) {
  const {checked, onChange} = props
  return (
    <div className="checkbox">
      <input type="checkbox" checked={checked} onChange={onChange} className="mr-2" />
      <label>
        Search everywhere (this may make the page unresponsive for a while, but eventually it
        finishes)
      </label>
    </div>
  )
}

export function SearchView(props: {view: QueryKey}) {
  const {build, allTargets} = useContext(DataContext)
  const {params} = useContext(RouterContext)

  const [universalSearch, setUniversalSearch] = useState(false)
  function handleChange(event: {target: {checked: boolean | ((prevState: boolean) => boolean)}}) {
    setUniversalSearch(event.target.checked)
    if (build != null) {
      if (indexCache) {
        return
      } else {
        indexEverything(build)
      }
    }
  }

  const urlParams = new URLSearchParams(params)
  const search = urlParams.get(props.view)

  if (search == null || search.length < 3) {
    return <p>Invalid search "{search}", try again</p>
  }

  let res = null
  if (universalSearch) {
    res = indexCache?.search(search)
  } else {
    res = []
    for (let k of Object.keys(allTargets)) {
      if (k.includes(search)) {
        res.push(k)
      }
    }
  }

  if (res == null || res.length == 0) {
    return (
      <>
        <Checkbox checked={universalSearch} onChange={handleChange} />
        <p>No results for search</p>
      </>
    )
  }

  // Not sure where the dups are coming from, but we want to dedup to prevent
  // undefined behavior in React
  const deduped = dedupeArray(res.map(v => v.toString()))

  return (
    <>
      <SearchBox />
      <Checkbox checked={universalSearch} onChange={handleChange} />
      <h5 className="title is-5 mt-4">Showing targets labels containing "{search}"</h5>
      <ul>
        {deduped.map(label => (
          <li key={label} className="mt-3">
            <Link to={{target: label}}>{label}</Link>
          </li>
        ))}
      </ul>
    </>
  )
}

function dedupeArray(res: string[]): string[] {
  const array = res.map((value, index) => [value, index])
  const deduped = Array.from(
    new Map(array as Iterable<readonly [unknown, unknown]>).keys(),
  ) as string[]
  return deduped
}
