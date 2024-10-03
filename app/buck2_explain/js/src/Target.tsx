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
import {ConfiguredTargetNode, TargetValueType, TargetField, TargetValue} from './fbs/explain'
import {Link} from './Router'

const TARGET_ATTRS = 'target_attrs'
const TARGET_DEPS = 'target_deps'
const TARGET_RDEPS = 'target_rdeps'

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
        "<Link to={{target: value}}>{value}</Link>",
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

function ListAttr(props: {
  getItem: (i: number) => TargetValue | null
  length: number
}): JSX.Element {
  const {getItem, length} = props
  if (length === 0) {
    return <span></span>
  }

  const items: JSX.Element[] = []
  for (let i = 0; i < props.length; i++) {
    items.push(
      <li key={i}>
        <Attr value={getItem(i)} />
      </li>,
    )
  }
  return <ul>[{items}]</ul>
}

function DictAttr(props: {
  getItem: (i: number) => TargetValue | null
  length: number
}): JSX.Element {
  const {getItem, length} = props
  if (length === 0) {
    return <span>[]</span>
  }

  const items: JSX.Element[] = []
  for (let i = 0; i < props.length; i++) {
    let value = getItem(i)
    items.push(
      <li key={i}>
        <Attr value={value?.key()} />:
        <Attr value={value} />
      </li>,
    )
  }
  return <ul>&#123;{items}&#125;</ul>
}

function Attr(props: {value: TargetValue | null | undefined}): JSX.Element {
  const {value} = props
  if (value == null) {
    return <>null value</>
  }
  const valueType = value?.type()
  if (valueType == null) {
    return <>null value type</>
  }
  switch (valueType) {
    case TargetValueType.Bool:
      return <>{value.boolValue()?.toString()}</>
    case TargetValueType.Int:
      return <>{value.intValue()?.toString()}</>
    case TargetValueType.String:
      // TODO iguridi: update this once we have ConfiguredTargetLabel type
      return <PossibleLink value={value.stringValue() || ''} />
    case TargetValueType.List:
      return <ListAttr getItem={i => value.listValue(i)} length={value.listValueLength()} />
    case TargetValueType.Dict:
      return <DictAttr getItem={i => value.dictValue(i)} length={value.dictValueLength()} />
  }
}

function Attrs(props: {attr: (i: number) => TargetField | null; length: number}): JSX.Element {
  const items: JSX.Element[] = []
  for (let i = 0; i < props.length; i++) {
    const attr = props.attr(i)
    if (attr == null) {
      continue
    }
    const row = (
      <li key={i}>
        {attr.name()} = <Attr value={attr.value()} />
      </li>
    )
    items.push(row)
  }
  return <>{items}</>
}

export function Target(props: {target: ConfiguredTargetNode; tab: string | null}) {
  const target = props.target
  const tab = props.tab ?? TARGET_ATTRS

  const filePath = target.codePointer()?.filePath()
  const lineNumber = (target.codePointer()?.line() ?? 0) + 1
  // TODO iguridi: make it work outside of fbsource
  const codePointer = `https://www.internalfb.com/code/fbsource/${filePath}?lines=${lineNumber}`

  const targetLabel = target.configuredTargetLabel()?.split(' ')[0]

  return (
    <div>
      <div className="ml-4 mt-4 mb-6">
        <a href={codePointer}>(codehub) </a>
        <h4 className="title is-4">{targetLabel}</h4>
        <ul>
          <li>
            <b>Rule type</b>
            <p className="mb-2 is-family-monospace">{target.type()}</p>
          </li>
        </ul>
      </div>
      <div className="tabs">
        <ul>
          <li className={tab === TARGET_ATTRS ? 'is-active' : ''}>
            <Link
              className="no-underline icon-text"
              to={{target: target.configuredTargetLabel(), target_tab: TARGET_ATTRS}}>
              Attributes
              <span className="icon">
                <i className="fa fa-list"></i>
              </span>
            </Link>
          </li>
          <li className={tab === TARGET_DEPS ? 'is-active' : ''}>
            <Link
              className="no-underline"
              to={{target: target.configuredTargetLabel(), target_tab: TARGET_DEPS}}>
              Dependencies
              <span className="icon">
                <i className="fa fa-arrow-down"></i>
              </span>
            </Link>
          </li>
          <li className={tab === TARGET_RDEPS ? 'is-active' : ''}>
            <Link
              className="no-underline"
              to={{target: target.configuredTargetLabel(), target_tab: TARGET_RDEPS}}>
              Reverse dependencies
              <span className="icon">
                <i className="fa fa-arrow-up"></i>
              </span>
            </Link>
          </li>
        </ul>
      </div>
      {tab === TARGET_ATTRS ? <TargetAttrs target={target} /> : null}
      {tab === TARGET_DEPS ? <TargetDeps target={target} /> : null}
      {tab === TARGET_RDEPS ? <TargetRdeps target={target} /> : null}
    </div>
  )
}

function TargetDeps(props: {target: ConfiguredTargetNode}) {
  const {target} = props
  return (
    <div className="is-family-monospace">
      deps = [<List attr={i => target.deps(i)} length={target.depsLength()} />]
    </div>
  )
}

function TargetRdeps(props: {target: ConfiguredTargetNode}) {
  const {target} = props
  const {allTargets, build} = useContext(DataContext)

  if (allTargets == null || build == null) {
    return
  }

  const label = target?.configuredTargetLabel()

  let rdeps: Array<string> = []
  Object.values(allTargets).forEach(i => {
    let target2 = build?.targets(i)
    let depsLength = target2?.depsLength() ?? 0
    for (let i = 0; i < depsLength; i++) {
      const dep = target2?.deps(i)
      const rdepLabel = target2?.configuredTargetLabel()
      if (dep === label && rdepLabel != null) {
        rdeps.push(rdepLabel)
      }
    }
  })

  return (
    <div className="is-family-monospace">
      rdeps = [
      <List attr={i => rdeps[i]} length={rdeps.length} />]
    </div>
  )
}

function TargetAttrs(props: {target: ConfiguredTargetNode}) {
  const {target} = props
  return (
    <ul className="is-family-monospace ml-4">
      <li>name = "{target.name()}",</li>
      <li>type = "{target.type()}",</li>
      <li>package = "{target.package_()}",</li>
      <li>oncall = "{target.oncall()}",</li>
      <li>target_configuration = "{target.targetConfiguration()}",</li>
      <li>execution_platform = "{target.executionPlatform()}",</li>
      <Attrs attr={i => target.attrs(i)} length={target.attrsLength()} />
    </ul>
  )
}
