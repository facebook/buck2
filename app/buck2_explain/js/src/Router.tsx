/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {ReactNode, useEffect, useState} from 'react'

export const SEARCH_VIEW = 'search'
export const TARGET_VIEW = 'target'
export const ROOT_VIEW = ''

export const RouterContext = React.createContext({params: '', setParams: (_s: string) => {}})

/**
 * Decides what to show based on existing query params
 * Inspired by reach-router library
 */
export function Router(props: {children: ReactNode}) {
  const [params, setUrlParams] = useState(window.location.search)

  const setParams = (s: string) => {
    const url = new URL(window.location.toString())
    const params = new URLSearchParams(s)
    url.search = params.toString()
    window.history.pushState(null, '', url.toString())
    setUrlParams(s)
  }

  useEffect(() => {
    const handlePopState = () => {
      setParams(document.location.search)
    }
    window.addEventListener('popstate', handlePopState)
    return () => {
      window.removeEventListener('popstate', handlePopState)
    }
  }, [])

  const urlParams = new URLSearchParams(params)
  const all = Array.from(urlParams.keys())

  const res = React.Children.map(props.children, child => {
    if (React.isValidElement(child)) {
      if (child.props.view === undefined) {
        return child
      }
      if (urlParams.has(child.props.view)) {
        return child
      }
      if (child.props.view === ROOT_VIEW && all.length === 0) {
        return child
      }
    }
    return null
  })

  return res?.length ? (
    <RouterContext.Provider value={{params, setParams: p => setParams(p)}}>
      {res}
    </RouterContext.Provider>
  ) : (
    <p>View not found</p>
  )
}

/**
 * Link with specified query params
 */
export function Link(props: {to: Map<string, string>; children: ReactNode}) {
  const {to, children} = props

  const url = new URL(window.location.toString())
  const params = new URLSearchParams(url.search)

  for (let [k, _] of params.entries()) {
    params.delete(k)
  }

  for (let [k, v] of to) {
    params.set(k, v)
  }

  url.search = params.toString()

  return <a href={url.toString()}>{children}</a>
}
