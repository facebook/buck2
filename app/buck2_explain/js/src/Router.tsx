/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {ReactNode, useContext, useEffect, useState} from 'react'

export enum QueryKey {
  RootView = '',
  SearchView = 'search',
  GraphView = 'graph',
  GraphView2 = 'graph2',
  TargetView = 'target',
  TargetTab = 'target_tab',
}

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
      setUrlParams(document.location.search)
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
      if (child.props.view === QueryKey.RootView && all.length === 0) {
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
export function Link(props: {
  to: {[key in QueryKey]?: string | null}
  children: ReactNode
  className?: string
}) {
  const {setParams} = useContext(RouterContext)

  const {to, children} = props

  const url = new URL(window.location.toString())
  const params = new URLSearchParams(url.search)

  for (let [k, _] of params.entries()) {
    params.delete(k)
  }

  for (let k of Object.keys(to)) {
    params.set(k, to[k as QueryKey] ?? '')
  }

  url.search = params.toString()

  const handleClick = (e: React.MouseEvent<HTMLAnchorElement>, url: URL) => {
    e.preventDefault()
    setParams(url.search)
  }

  return (
    <a className={props.className} href={url.toString()} onClick={e => handleClick(e, url)}>
      {children}
    </a>
  )
}
