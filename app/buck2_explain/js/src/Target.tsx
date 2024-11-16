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
import {ConfiguredTargetNode, TargetValueType, TargetValue} from './fbs/explain'
import {Link} from './Router'
import {formatTargetLabel} from './formatTargetLabel'

const TARGET_ATTRS = 'target_attrs'
const TARGET_DEPS = 'target_deps'
const TARGET_RDEPS = 'target_rdeps'
const TARGET_ACTIONS = 'target_actions'

/*
 * If we have an object associated with this string, make it a link.
 * Otherwise, just render the string.
 */
function PossibleLink(props: {value: string}) {
  const {allTargets} = useContext(DataContext)
  const {value} = props

  let res = null
  if (allTargets.hasOwnProperty(value)) {
    res = <Link to={{target: value}}>{value}</Link>
  } else {
    res = <>{value}</>
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
        <PossibleLink value={props.attr(i).trim()} />
      </li>,
    )
  }
  return (
    <div className="content">
      <ul>{items}</ul>
    </div>
  )
}

export function Target(props: {target: ConfiguredTargetNode; tab: string | null}) {
  const target = props.target!
  const tab = props.tab ?? TARGET_ATTRS

  const filePath = target.codePointer()?.filePath()
  const lineNumber = (target.codePointer()?.line() ?? 0) + 1
  // TODO iguridi: make it work outside of fbsource
  const codePointer = `https://www.internalfb.com/code/fbsource/${filePath}?lines=${lineNumber}`

  const targetLabel = target.label()!.targetLabel()

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
              to={{target: formatTargetLabel(target.label()!), target_tab: TARGET_ATTRS}}>
              Attributes
              <span className="icon">
                <i className="fa fa-list"></i>
              </span>
            </Link>
          </li>
          <li className={tab === TARGET_DEPS ? 'is-active' : ''}>
            <Link
              className="no-underline"
              to={{target: formatTargetLabel(target.label()!), target_tab: TARGET_DEPS}}>
              Dependencies
              <span className="icon">
                <i className="fa fa-arrow-down"></i>
              </span>
            </Link>
          </li>
          <li className={tab === TARGET_RDEPS ? 'is-active' : ''}>
            <Link
              className="no-underline"
              to={{target: formatTargetLabel(target.label()!), target_tab: TARGET_RDEPS}}>
              Reverse dependencies
              <span className="icon">
                <i className="fa fa-arrow-up"></i>
              </span>
            </Link>
          </li>
          <li className={tab === TARGET_ACTIONS ? 'is-active' : ''}>
            <Link
              className="no-underline"
              to={{target: formatTargetLabel(target.label()!), target_tab: TARGET_ACTIONS}}>
              Actions
              <span className="icon">
                <i className="fa fa-tasks"></i>
              </span>
            </Link>
          </li>
        </ul>
      </div>
      {tab === TARGET_ATTRS ? <TargetAttrs target={target} /> : null}
      {tab === TARGET_DEPS ? <TargetDeps target={target} /> : null}
      {tab === TARGET_RDEPS ? <TargetRdeps target={target} /> : null}
      {tab === TARGET_ACTIONS ? <TargetActions target={target} /> : null}
    </div>
  )
}

function TargetDeps(props: {target: ConfiguredTargetNode}) {
  const {target} = props
  return <List attr={i => formatTargetLabel(target!.deps(i)!)} length={target.depsLength()} />
}

function TargetActions(props: {target: ConfiguredTargetNode}) {
  const {target} = props
  let repros = []
  for (let i = 0; i < target.actionsLength(); i++) {
    const action = target.actions(i)!
    for (let j = 0; j < action.reprosLength(); j++) {
      repros.push(action.repros(j)!)
    }
  }
  let res = repros.map(r => <ul>{r}</ul>)
  return <div className="content">{res}</div>
}

function TargetRdeps(props: {target: ConfiguredTargetNode}) {
  const {target} = props
  const {allTargets, build} = useContext(DataContext)

  if (allTargets == null || build == null) {
    return
  }

  const label = formatTargetLabel(target!.label()!)

  let rdeps: Array<string> = []
  Object.values(allTargets).forEach(i => {
    let target2 = build?.targets(i)
    let depsLength = target2?.depsLength() ?? 0
    for (let i = 0; i < depsLength; i++) {
      const dep = formatTargetLabel(target2?.deps(i)!)
      const rdepLabel = formatTargetLabel(target2!.label()!)
      if (dep === label) {
        rdeps.push(rdepLabel)
      }
    }
  })

  return <List attr={i => rdeps[i]} length={rdeps.length} />
}

function TargetAttrs(props: {target: ConfiguredTargetNode}) {
  const {target} = props
  return (
    <table className="table ml-4">
      <tbody>
        <tr>
          <th>Attribute</th>
          <th>Value</th>
        </tr>
        <tr>
          <td>name</td>
          <td>{target.name()}</td>
        </tr>
        <tr>
          <td>type</td>
          <td>{target.type()}</td>
        </tr>
        <tr>
          <td>package</td>
          <td>{target.package_()}</td>
        </tr>
        <tr>
          <td>oncall</td>
          <td>{target.oncall()}</td>
        </tr>
        <tr>
          <td>target_configuration</td>
          <td>{target.targetConfiguration()}</td>
        </tr>
        <tr>
          <td>execution_platform</td>
          <td>{target.executionPlatform()}</td>
        </tr>
      </tbody>
    </table>
  )
}
