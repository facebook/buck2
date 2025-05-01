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
import {indexCache, indexEverything, searchableText} from './flexSearch'
import {HighlightedText, searchRegex} from './HighlightedText'
import {formatTargetLabel} from './formatTargetLabel'

function SearchResult(props: {id: number; regex: RegExp}) {
  const {id, regex} = props
  const {build} = useContext(DataContext)
  const target = build?.targets(id)!

  const label = target.label()!
  const searchables = searchableText(target)

  const matches = searchables
    .filter(s => regex.test(s))
    .map((s, i) => (
      <span key={i} className="has-text-grey">
        . . .
        <HighlightedText text={s} regex={regex} />. . .{'  '}
      </span>
    ))

  return (
    <li key={id} className="mt-3">
      <Link to={{target: formatTargetLabel(label)}} className="is-size-5">
        {/* TODO iguridi: show configuration for targets built in multiple configurations */}
        <HighlightedText text={label.targetLabel()!} regex={regex} />
      </Link>
      <br />
      {matches}
    </li>
  )
}

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
  let res = indexCache!.search(search) as number[]

  // This regex is used to highlight results
  const regex = searchRegex(search)

  const view =
    res == null || res.length == 0 ? (
      <>
        <p>No results for search</p>
      </>
    ) : (
      <>
        <h5 className="title is-5 mt-4">
          Showing {res.length} targets containing "{search}"
        </h5>
        <ul>
          {res.map(i => (
            <SearchResult key={i} id={i} regex={regex} />
          ))}
        </ul>
      </>
    )

  return <div className="mx-4">{view}</div>
}
