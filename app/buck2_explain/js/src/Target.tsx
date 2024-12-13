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
const TARGET_CHANGED_FILES = 'target_changed_files'

/*
 * If we have an object associated with this string, make it a link.
 * Otherwise, just render the string.
 */
function PossibleLink(props: {value: string}) {
  const {allTargets, graph} = useContext(DataContext)
  const {value} = props

  let res = null
  let exists = allTargets.get(value)
  if (exists != null && graph.has(exists)) {
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
  const codePointer =
    filePath != null && lineNumber != null ? (
      <a href={`https://www.internalfb.com/code/fbsource/${filePath}?lines=${lineNumber}`}>
        (codehub)
      </a>
    ) : null

  const unconfiguredLabel = target.label()!.targetLabel()
  const configuredLabel = formatTargetLabel(target.label()!)

  const getRdeps = (label: string) => {
    const {allTargets, build, graph} = useContext(DataContext)
    if (allTargets == null || build == null) {
      return []
    }

    return graph
      .get(allTargets.get(label)!)!
      .rdeps.map(i => formatTargetLabel(build!.targets(i)!.label()!))
  }
  const rdeps: Array<string> = getRdeps(configuredLabel)

  return (
    <div>
      <div className="ml-4 mt-4 mb-6">
        {codePointer}
        <h4 className="title is-4">{unconfiguredLabel}</h4>
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
              to={{target: configuredLabel, target_tab: TARGET_ATTRS}}>
              <span className="icon">
                <i className="fa fa-list"></i>
              </span>
              Attributes
            </Link>
          </li>
          <li className={tab === TARGET_DEPS ? 'is-active' : ''}>
            <Link className="no-underline" to={{target: configuredLabel, target_tab: TARGET_DEPS}}>
              <span className="icon">
                <i className="fa fa-arrow-down"></i>
              </span>
              Dependencies ({target.depsLength()})
            </Link>
          </li>
          <li className={tab === TARGET_RDEPS ? 'is-active' : ''}>
            <Link className="no-underline" to={{target: configuredLabel, target_tab: TARGET_RDEPS}}>
              <span className="icon">
                <i className="fa fa-arrow-up"></i>
              </span>
              Reverse dependencies ({rdeps.length})
            </Link>
          </li>
          <li className={tab === TARGET_ACTIONS ? 'is-active' : ''}>
            <Link
              className="no-underline"
              to={{target: configuredLabel, target_tab: TARGET_ACTIONS}}>
              <span className="icon">
                <i className="fa fa-tasks"></i>
              </span>
              Actions ({target.actionsLength()})
            </Link>
          </li>
          <li className={tab === TARGET_CHANGED_FILES ? 'is-active' : ''}>
            <Link
              className="no-underline"
              to={{target: configuredLabel, target_tab: TARGET_CHANGED_FILES}}>
              <span className="icon">
                <i className="fa fa-file"></i>
              </span>
              Changed files ({target.changedFilesLength()})
            </Link>
          </li>
        </ul>
      </div>
      {tab === TARGET_ATTRS ? <TargetAttrs target={target} /> : null}
      {tab === TARGET_DEPS ? <TargetDeps target={target} /> : null}
      {tab === TARGET_RDEPS ? <List attr={i => rdeps[i]} length={rdeps.length} /> : null}
      {tab === TARGET_ACTIONS ? <TargetActions target={target} /> : null}
      {tab === TARGET_CHANGED_FILES ? <TargetChangedFiles target={target} /> : null}
    </div>
  )
}

function TargetDeps(props: {target: ConfiguredTargetNode}) {
  const {target} = props
  return <List attr={i => formatTargetLabel(target!.deps(i)!)} length={target.depsLength()} />
}

function TargetActions(props: {target: ConfiguredTargetNode}) {
  const {target} = props
  let rows = []

  for (let i = 0; i < target.actionsLength(); i++) {
    const action = target.actions(i)!
    const repros: string[] = []
    for (let j = 0; j < action.reprosLength(); j++) {
      repros.push(action.repros(j)!)
    }
    rows.push({
      category: action.category(),
      identifier: action.identifier(),
      failed: action.failed(),
      executionKind: action.executionKind(),
      inputFilesBytes: action.inputFilesBytes(),
      affectedByFileChanges: action.affectedByFileChanges(),
      repros,
    })
  }

  return (
    <table className="table ml-4">
      <tbody>
        <tr>
          <th>Category</th>
          <th>Identifier</th>
          <th>Failed</th>
          <th>Execution kind</th>
          <th>Input files bytes</th>
          <th>Affected by file changes</th>
          <th>Repros</th>
        </tr>
        {rows.map(a => (
          <tr>
            <td>{a.category}</td>
            <td>{a.identifier}</td>
            <td>{a.failed.toString()}</td>
            <td>{a.executionKind}</td>
            <td>{a.inputFilesBytes?.toString()}</td>
            <td>{a.affectedByFileChanges.toString()}</td>
            <td>
              <ul>
                {a.repros.map(r => (
                  <li>
                    <code>{r}</code>
                  </li>
                ))}
              </ul>
            </td>
          </tr>
        ))}
      </tbody>
    </table>
  )
}

function TargetChangedFiles(props: {target: ConfiguredTargetNode}) {
  const {target} = props
  let files = []
  for (let i = 0; i < target.changedFilesLength(); i++) {
    const file = target.changedFiles(i)!
    files.push(file)
  }
  let res = files.map(r => <ul>{r}</ul>)
  return <div className="content">{res}</div>
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
