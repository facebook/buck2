/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {ReactNode} from 'react'

export const SEARCH_VIEW = 'search'
export const TARGET_VIEW = 'target'
export const ROOT_VIEW = ''

/**
 * Decides what to show based on existing query params
 * Inspired by reach-router library
 */
export function Router(props: {children: ReactNode}) {
  const params = new URLSearchParams(window.location.search)
  const all = Array.from(params.keys())

  const res = React.Children.map(props.children, child => {
    if (React.isValidElement(child)) {
      if (params.has(child.props.view)) {
        return child
      }

      if (child.props.view === ROOT_VIEW && all.length === 0) {
        return child
      }
    }
    return null
  })

  return res && res[0] ? res[0] : <p>View not found</p>
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
