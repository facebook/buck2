/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Add css to the bundle
import './app.css'

import React, {useEffect, useState} from 'react'
import {createRoot} from 'react-dom/client'

import {ByteBuffer} from 'flatbuffers'
import {Build, ConfiguredTargetNode} from './fbs/explain'
import {QueryKey, ROOT_VIEW, Router} from './Router'
import {Index} from 'flexsearch-ts'
import {RootView} from './RootView'
import {TargetView} from './TargetView'
import {SearchView} from './SearchView'
import {GraphView} from './graph/GraphView'
import {Header} from './Header'

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

export const DataContext = React.createContext<STATE_TYPE>(INITIAL_STATE)

function App() {
  const [data, setData] = useState<STATE_TYPE>(INITIAL_STATE)

  /**
   * Loads initial information on page load
   */
  useEffect(() => {
    const fetchData = async () => {
      // keep this line as is, it will be replaced later
      let blobBase64 = 'XXDATAXX'
      try {
        blobBase64 = (await import('./data')).DATA
      } catch (error) {
        console.info('./data.ts not found, using replaced data')
      }

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

      // This should run just once total
      setData({build, allTargets, rootTarget})
    }
    fetchData()
  }, [])

  const rootTarget = data.rootTarget

  if (rootTarget == null) return <p>Loading...</p>
  else {
    return (
      <DataContext.Provider value={data}>
        <Router>
          <Header />
          <RootView view={ROOT_VIEW} />
          <TargetView view={QueryKey.TargetView} />
          <SearchView view={QueryKey.SearchView} />
          <GraphView view={QueryKey.GraphView} />
        </Router>
      </DataContext.Provider>
    )
  }
}

const container = document.getElementById('root') as HTMLElement
const root = createRoot(container)

root.render(<App />)
