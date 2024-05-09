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
import {Build, ConfiguredTargetNode} from './fbs/explain'
import {Target} from './Target'

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

  const allTargets: {[key: string]: number} = {}
  for (let i = 0; i < build.targetsLength(); i++) {
    let target = build.targets(i)
    let label = target?.configuredTargetLabel()
    if (label == null) {
      continue
    }
    allTargets[label] = i
  }

  const params = new URLSearchParams(window.location.search)
  const target_param = params.get('target')

  let target = target_param == null ? null : build.targets(allTargets[target_param])
  target = target || rootTarget

  if (target != null && target?.configuredTargetLabel() != currentTarget?.configuredTargetLabel()) {
    setCurrentTarget(target)
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
        <Target target={currentTarget} />
      </>
    )
  }
}

const container = document.getElementById('root') as HTMLElement
const root = createRoot(container)

root.render(<App />)
