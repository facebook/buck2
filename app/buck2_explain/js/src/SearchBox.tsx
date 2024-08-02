/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useContext, useState} from 'react'
import {RouterContext, SEARCH_VIEW} from './Router'

export function SearchBox() {
  const {params, setParams} = useContext(RouterContext)
  const urlParams = new URLSearchParams(params)

  const [searchTerm, setSearchTerm] = useState(urlParams.get(SEARCH_VIEW) ?? '')

  const goSearch = () => {
    const url = new URL(window.location.toString())
    const params = new URLSearchParams(url.search)

    for (let [k, _] of params.entries()) {
      params.delete(k)
    }

    params.set(SEARCH_VIEW, searchTerm)

    setParams(params.toString())
  }
  return (
    <div className="is-flex is-flex-direction-row">
      <input
        type="text"
        className="input is-one-fifth column mr-2 is-small"
        value={searchTerm}
        onChange={event => setSearchTerm(event.target.value)}
        placeholder="Search targets"
        onKeyDown={event => {
          event.key == 'Enter' ? goSearch() : null
        }}
      />
      <button type="submit" onPointerDown={goSearch} className="button is-dark is-small">
        <span>Search</span>
        <span className="icon">
          <i className="fa fa-search"></i>
        </span>
      </button>
    </div>
  )
}
