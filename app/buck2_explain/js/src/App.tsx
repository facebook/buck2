/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useEffect, useState} from 'react'
import {createRoot} from 'react-dom/client'

import {ByteBuffer} from 'flatbuffers'
import {BoolAttr, Build, ConfiguredTargetNode, ListOfStringsAttr, StringAttr} from './fbs/explain'

function List(props: {attr: (i: number) => String; length: number}): JSX.Element {
  const items: JSX.Element[] = []
  for (let i = 0; i < props.length; i++) {
    const value = props.attr(i)
    const row = <li key={i}>{value}</li>
    items.push(row)
  }
  return <ul>{items}</ul>
}

function ListOfBoolAttrs(props: {
  attr: (i: number) => BoolAttr | null
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
        <b>{value.key()}: </b>
        <span>{value.value()}</span>
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
        <b>{value.key()}: </b>
        <span>{value.value()}</span>
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
        <b>{value.key()}: </b>
        <List attr={i => value.value(i)} length={value.valueLength()} />
      </li>
    )
    items.push(row)
  }
  return <>{items}</>
}

function App() {
  const [blobBase64, setBlobBase64] = useState('XXDATAXX')
  const [currentTarget, setCurrentTarget] = useState<ConfiguredTargetNode | null>(null)

  useEffect(() => {
    const fetchData = async () => {
      try {
        // keep this line as is, it will be replaced later
        const data = (await import('./data')).DATA
        setBlobBase64(data)
      } catch (error) {
        console.error('Error:', error)
      }
    }
    fetchData()
  }, [])

  const decodedString = atob(blobBase64)
  // TODO iguridi: decode blob better
  const byteArray = new Uint8Array(decodedString.length)
  for (let i = 0; i < decodedString.length; i++) {
    byteArray[i] = decodedString.charCodeAt(i)
  }

  let buf = new ByteBuffer(byteArray)

  // Get an accessor to the root object inside the buffer.
  const build = Build.getRootAsBuild(buf)

  const rootTarget = build.targets(0)

  if (currentTarget == null && rootTarget != null) {
    setCurrentTarget(rootTarget)
  }

  if (currentTarget == null || rootTarget == null) return <p>Loading...</p>
  else {
    return (
      <>
        <p>
          <i>
            <span>{rootTarget.configuredTargetLabel()}</span>
          </i>
        </p>
        <ul>
          <li>
            <b>Name: </b>
            <span>{currentTarget.name()}</span>
          </li>
          <li>
            <b>Default target platform: </b>
            <span>{currentTarget.defaultTargetPlatform()}</span>
          </li>
          <li>
            <b>Target compatible with: </b>
            <List
              attr={i => currentTarget.targetCompatibleWith(i)}
              length={currentTarget.targetCompatibleWithLength()}
            />
          </li>
          <li>
            <b>Compatible with: </b>
            <List
              attr={i => currentTarget.compatibleWith(i)}
              length={currentTarget.compatibleWithLength()}
            />
          </li>
          <li>
            <b>Exec compatible with: </b>
            <List
              attr={i => currentTarget.execCompatibleWith(i)}
              length={currentTarget.execCompatibleWithLength()}
            />
          </li>
          <li>
            <b>Visibility: </b>
            <List
              attr={i => currentTarget.visibility(i)}
              length={currentTarget.visibilityLength()}
            />
          </li>
          <li>
            <b>Within view: </b>
            <List
              attr={i => currentTarget.withinView(i)}
              length={currentTarget.withinViewLength()}
            />
          </li>
          <li>
            <b>Tests: </b>
            <List attr={i => currentTarget.tests(i)} length={currentTarget.testsLength()} />
          </li>
          <li>
            <b>Type: </b>
            <span>{currentTarget.type()}</span>
          </li>
          <li>
            <b>Deps: </b>
            <List attr={i => currentTarget.deps(i)} length={currentTarget.depsLength()} />
          </li>
          <li>
            <b>Package: </b>
            <span>{currentTarget.package_()}</span>
          </li>
          <li>
            <b>Oncall: </b>
            <span>{currentTarget.oncall()}</span>
          </li>
          <li>
            <b>Target configuration: </b>
            <span>{currentTarget.targetConfiguration()}</span>
          </li>
          <li>
            <b>Execution platform: </b>
            <span>{currentTarget.executionPlatform()}</span>
          </li>
          <li>
            <b>Plugins: </b>
            <List attr={i => currentTarget.plugins(i)} length={currentTarget.pluginsLength()} />
          </li>
          <ListOfBoolAttrs
            attr={i => currentTarget.boolAttrs(i)}
            length={currentTarget.boolAttrsLength()}
          />
          <ListOfStringAttrs
            attr={i => currentTarget.stringAttrs(i)}
            length={currentTarget.stringAttrsLength()}
          />
          <ListOfListOfStringAttrs
            attr={i => currentTarget.listOfStringsAttrs(i)}
            length={currentTarget.listOfStringsAttrsLength()}
          />
        </ul>
      </>
    )
  }
}

const container = document.getElementById('root') as HTMLElement
const root = createRoot(container)

root.render(<App />)
