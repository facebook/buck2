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
import {Build} from './fbs/explain'

function List(props: {attr: (i: number) => String; length: number}): JSX.Element {
  const items: JSX.Element[] = []
  for (let i = 0; i < props.length; i++) {
    const value = props.attr(i)
    const row = <li key={i}>{value}</li>
    items.push(row)
  }
  return <ul>{items}</ul>
}

function App() {
  const [blobBase64, setBlobBase64] = useState('XXDATAXX')
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

  if (rootTarget == null) return <p>Loading...</p>
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
            <span>{rootTarget.name()}</span>
          </li>
          <li>
            <b>Default target platform: </b>
            <span>{rootTarget.defaultTargetPlatform()}</span>
          </li>
          <li>
            <b>Target compatible with: </b>
            <List
              attr={i => rootTarget.targetCompatibleWith(i)}
              length={rootTarget.targetCompatibleWithLength()}
            />
          </li>
          <li>
            <b>Compatible with: </b>
            <List
              attr={i => rootTarget.compatibleWith(i)}
              length={rootTarget.compatibleWithLength()}
            />
          </li>
          <li>
            <b>Exec compatible with: </b>
            <List
              attr={i => rootTarget.execCompatibleWith(i)}
              length={rootTarget.execCompatibleWithLength()}
            />
          </li>
          <li>
            <b>Visibility: </b>
            <List attr={i => rootTarget.visibility(i)} length={rootTarget.visibilityLength()} />
          </li>
          <li>
            <b>Within view: </b>
            <List attr={i => rootTarget.withinView(i)} length={rootTarget.withinViewLength()} />
          </li>
          <li>
            <b>Tests: </b>
            <List attr={i => rootTarget.tests(i)} length={rootTarget.testsLength()} />
          </li>
          <li>
            <b>Type: </b>
            <span>{rootTarget.type()}</span>
          </li>
          <li>
            <b>Deps: </b>
            <List attr={i => rootTarget.deps(i)} length={rootTarget.depsLength()} />
          </li>
          <li>
            <b>Package: </b>
            <span>{rootTarget.package_()}</span>
          </li>
          <li>
            <b>Oncall: </b>
            <span>{rootTarget.oncall()}</span>
          </li>
          <li>
            <b>Target configuration: </b>
            <span>{rootTarget.targetConfiguration()}</span>
          </li>
          <li>
            <b>Execution platform: </b>
            <span>{rootTarget.executionPlatform()}</span>
          </li>
          <li>
            <b>Plugins: </b>
            <List attr={i => rootTarget.plugins(i)} length={rootTarget.pluginsLength()} />
          </li>
        </ul>
      </>
    )
  }
}

const container = document.getElementById('root') as HTMLElement
const root = createRoot(container)

root.render(<App />)
