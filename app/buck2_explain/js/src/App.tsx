/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {Dispatch, SetStateAction, useEffect, useState} from 'react'
import {createRoot} from 'react-dom/client'

import {ByteBuffer} from 'flatbuffers'
import {Build, ConfiguredTargetNode} from './fbs/explain'
import {Target} from './Target'

const INITIAL_STATE = {
  build: null,
  rootTarget: null,
  allTargets: {},
}

type STATE_TYPE = {
  build: Build | null
  rootTarget: ConfiguredTargetNode | null
  allTargets: {[key: string]: number}
}

function fromUrl(
  data: STATE_TYPE,
  currentTarget: ConfiguredTargetNode | null,
  setCurrentTarget: Dispatch<SetStateAction<ConfiguredTargetNode | null>>,
) {
  const {build, allTargets, rootTarget} = data
  const params = new URLSearchParams(window.location.search)
  const target_param = params.get('target')

  let target = target_param == null ? null : build?.targets(allTargets[target_param])
  target = target || rootTarget

  if (target != null && currentTarget?.configuredTargetLabel() != target?.configuredTargetLabel()) {
    setCurrentTarget(target)
  }
}

function RootSpan(props: {
  target: ConfiguredTargetNode
  setCurrentTarget: Dispatch<SetStateAction<ConfiguredTargetNode | null>>
}) {
  const handleClick = () => {
    const url = new URL(window.location.toString())
    const params = new URLSearchParams(url.search)
    params.delete('target')
    url.search = params.toString()
    window.history.pushState({}, '', url.toString())
    props.setCurrentTarget(props.target)
  }
  return (
    <p style={{cursor: 'pointer'}} onClick={handleClick}>
      <i>
        <span>{props.target.configuredTargetLabel()}</span>
      </i>
    </p>
  )
}

function App() {
  const [data, setData] = useState<STATE_TYPE>(INITIAL_STATE)

  const [currentTarget, setCurrentTarget] = useState<ConfiguredTargetNode | null>(null)

  useEffect(() => {
    // This is triggered every time the user goes back in history
    const f = () => {
      fromUrl(data, currentTarget, setCurrentTarget)
    }
    window.addEventListener('popstate', f)
    return () => {
      window.removeEventListener('popstate', f)
    }
  }, [data, currentTarget])

  useEffect(() => {
    const fetchData = async () => {
      try {
        // keep this line as is, it will be replaced later
        const data = (await import('./data')).DATA
        const blobBase64 = data
        const decodedString = atob(blobBase64)
        // TODO iguridi: decode blob better
        const byteArray = new Uint8Array(decodedString.length)
        for (let i = 0; i < decodedString.length; i++) {
          byteArray[i] = decodedString.charCodeAt(i)
        }

        const buf = new ByteBuffer(byteArray)

        // Get an accessor to the root object inside the buffer.
        const build = Build.getRootAsBuild(buf)
        // TODO iguridi: just show 1 target for now
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

        const data2 = {build, allTargets, rootTarget}

        setData(data2)

        // This should run just once total
        fromUrl(data2, currentTarget, setCurrentTarget)
      } catch (error) {
        console.error('Error:', error)
      }
    }
    fetchData()
  }, [])

  const rootTarget = data.rootTarget

  if (currentTarget == null || rootTarget == null) return <p>Loading...</p>
  else {
    return (
      <>
        <RootSpan target={rootTarget} setCurrentTarget={setCurrentTarget} />
        <Target target={currentTarget} />
      </>
    )
  }
}

const container = document.getElementById('root') as HTMLElement
const root = createRoot(container)

root.render(<App />)
