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
import {HighlightedText} from './HighlightedText'

export function SearchView(props: {view: QueryKey}) {
  const {params} = useContext(RouterContext)
  const {build, graph} = useContext(DataContext)
  if (build == null) {
    return null
  }

  const urlParams = new URLSearchParams(params)
  const search = urlParams.get(props.view)

  if (search == null || search.length < 3) {
    return <p>Invalid search "{search}", try again with a text of 3 or more characters</p>
  }

  if (!indexCache) {
    indexEverything(build, graph)
  }
  const res = indexCache!.search(search)

  // Not sure where the dups are coming from, but we want to dedup to prevent
  // undefined behavior in React
  const deduped = dedupeArray((res ?? []).map(v => v.toString()))

  const view =
    res == null || res.length == 0 ? (
      <>
        <p>No results for search</p>
      </>
    ) : (
      <>
        <h5 className="title is-5 mt-4">Showing targets containing "{search}"</h5>
        <ul>
          {deduped.map(label => (
            <li key={label} className="mt-3">
              <Link to={{target: label}}>
                <HighlightedText text={label} searchQuery={search} />
              </Link>
            </li>
          ))}
        </ul>
      </>
    )

  return <div className="mx-4">{view}</div>
}

function dedupeArray(res: string[]): string[] {
  const array = res.map((value, index) => [value, index])
  const deduped = Array.from(
    new Map(array as Iterable<readonly [unknown, unknown]>).keys(),
  ) as string[]
  return deduped
}
