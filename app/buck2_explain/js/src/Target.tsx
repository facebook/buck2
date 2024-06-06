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
import {IntAttr, BoolAttr, ConfiguredTargetNode, ListOfStringsAttr, StringAttr} from './fbs/explain'
import {Link, TARGET_VIEW} from './Router'

/*
 * If we have an object associated with this string, make it a link.
 * Otherwise, just render the string.
 */
function PossibleLink(props: {value: string}) {
  const {allTargets} = useContext(DataContext)
  const {value} = props

  let res = null
  if (allTargets.hasOwnProperty(value)) {
    res = (
      <>
        "<Link to={new Map().set(TARGET_VIEW, value)}>{value}</Link>",
      </>
    )
  } else {
    res = <>"{value}",</>
  }
  return res
}

function List(props: {attr: (i: number) => string; length: number}): JSX.Element {
  if (props.length === 0) {
    return <span>[]</span>
  }

  const items: JSX.Element[] = []
  for (let i = 0; i < props.length; i++) {
    items.push(
      <li key={i}>
        <PossibleLink value={props.attr(i)} />
      </li>,
    )
  }
  return <ul>{items}</ul>
}

function ListOfPlainAttrs(props: {
  attr: (i: number) => IntAttr | BoolAttr | null
  length: number
}): JSX.Element {
  const items: JSX.Element[] = []
  for (let i = 0; i < props.length; i++) {
    const value = props.attr(i)
    if (value == null) {
      continue
    }
    const row = (
      <li key={i}>
        {value.key()} = <PossibleLink value={value?.value()?.toString()} />
      </li>
    )
    items.push(row)
  }
  return <>{items}</>
}

function ListOfStringAttrs(props: {
  attr: (i: number) => StringAttr | null
  length: number
}): JSX.Element {
  const items: JSX.Element[] = []
  for (let i = 0; i < props.length; i++) {
    const value = props.attr(i)
    if (value == null) {
      continue
    }
    const row = (
      <li key={i}>
        {value.key()} = <PossibleLink value={value.value() || 'Empty plain attr'} />
      </li>
    )
    items.push(row)
  }
  return <>{items}</>
}

function ListOfListOfStringAttrs(props: {
  attr: (i: number) => ListOfStringsAttr | null
  length: number
}): JSX.Element {
  const items: JSX.Element[] = []
  for (let i = 0; i < props.length; i++) {
    const value = props.attr(i)
    if (value == null) {
      continue
    }
    const row = (
      <li key={i}>
        {value.key()} = [
        <List attr={i => value.value(i)} length={value.valueLength()} />
        ],
      </li>
    )
    items.push(row)
  }
  return <>{items}</>
}

export function Target(props: {target: ConfiguredTargetNode}) {
  const target = props.target
  return (
    <>
      <h3>{target.configuredTargetLabel()}</h3>

      <ul>
        <li>name = "{target.name()}",</li>
        <li>type = "{target.type()}",</li>
        <li>
          deps = [
          <List attr={i => target.deps(i)} length={target.depsLength()} />
          ],
        </li>
        <li>package = "{target.package_()}",</li>
        <li>oncall = "{target.oncall()}",</li>
        <li>target_configuration = "{target.targetConfiguration()}",</li>
        <li>execution_platform = "{target.executionPlatform()}",</li>
        <ListOfPlainAttrs attr={i => target.boolAttrs(i)} length={target.boolAttrsLength()} />
        <ListOfPlainAttrs attr={i => target.intAttrs(i)} length={target.intAttrsLength()} />
        <ListOfStringAttrs attr={i => target.stringAttrs(i)} length={target.stringAttrsLength()} />
        <ListOfListOfStringAttrs
          attr={i => target.listOfStringsAttrs(i)}
          length={target.listOfStringsAttrsLength()}
        />
      </ul>
    </>
  )
}
