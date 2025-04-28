/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useContext, useState} from 'react'
import {RouterContext, QueryKey} from './Router'

export function SearchBox() {
  const {params, setParams} = useContext(RouterContext)
  const urlParams = new URLSearchParams(params)

  const [searchTerm, setSearchTerm] = useState(urlParams.get(QueryKey.SearchView) ?? '')

  const goSearch = () => {
    const url = new URL(window.location.toString())
    const params = new URLSearchParams(url.search)

    for (let [k, _] of params.entries()) {
      params.delete(k)
    }

    params.set(QueryKey.SearchView, searchTerm)

    setParams(params.toString())
  }
  return (
    <>
      <input
        type="text"
        className="input mr-2 has-text-info-bold"
        value={searchTerm}
        onChange={event => setSearchTerm(event.target.value)}
        placeholder="Search"
        onKeyDown={event => {
          event.key == 'Enter' ? goSearch() : null
        }}
      />
      <button type="submit" onPointerDown={goSearch} className="button has-text-info-bold">
        <span>Search</span>
        <span className="icon">
          <i className="fa fa-search"></i>
        </span>
      </button>
    </>
  )
}
