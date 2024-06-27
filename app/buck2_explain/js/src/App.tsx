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
import {Index} from 'flexsearch-ts'
import {Build, ConfiguredTargetNode, TargetField, TargetValue, TargetValueType} from './fbs/explain'
import {ROOT_VIEW, Router, SEARCH_VIEW, TARGET_VIEW} from './Router'
import {RootView} from './RootView'
import {TargetView} from './TargetView'
import {SearchView} from './SearchView'
import {Header} from './Header'

const INITIAL_STATE = {
  build: null,
  rootTarget: null,
  allTargets: {},
  search_index: null,
}

type STATE_TYPE = {
  build: Build | null
  rootTarget: ConfiguredTargetNode | null
  allTargets: {[key: string]: number}
  search_index: Index | null
}

export const DataContext = React.createContext<STATE_TYPE>(INITIAL_STATE)

function addIfExists(index: Index, key: string, element: string | null | undefined) {
  if (element != null) {
    index.append(key, element)
  }
}

function addList(
  index: Index,
  key: string,
  attr: (i: number) => TargetValue | null | undefined,
  length: number,
) {
  for (let i = 0; i < length; i++) {
    addTargetValue(index, key, attr(i))
  }
}

function addListOfStrings(index: Index, key: string, attr: (i: number) => string, length: number) {
  for (let i = 0; i < length; i++) {
    const value = attr(i)
    if (value != null) {
      addIfExists(index, key, value)
    }
  }
}

function addDict(
  index: Index,
  key: string,
  attr: (i: number) => TargetValue | null | undefined,
  length: number,
) {
  for (let i = 0; i < length; i++) {
    const value = attr(i)
    addTargetValue(index, key, value?.key())
    addTargetValue(index, key, value)
  }
}

function addTargetField(index: Index, key: string, field: TargetField | null) {
  if (field == null) {
    return
  }
  const name = field?.name()
  addIfExists(index, key, name)
  addTargetValue(index, key, field?.value())
}

function addTargetValue(index: Index, key: string, value: TargetValue | null | undefined) {
  if (value == null) {
    return
  }
  const valueType = value?.type()
  if (valueType == null) {
    return
  }
  switch (valueType) {
    case TargetValueType.Bool:
      addIfExists(index, key, value.boolValue()?.toString())
    case TargetValueType.Int:
      addIfExists(index, key, value.intValue()?.toString())
    case TargetValueType.String:
      addIfExists(index, key, value.stringValue())
    case TargetValueType.List:
      addList(index, key, i => value.listValue(i), value.listValueLength())
    case TargetValueType.Dict:
      addDict(index, key, i => value.dictValue(i), value.dictValueLength())
  }
}

function addListOfTargetFields(
  index: Index,
  key: string,
  attr: (i: number) => TargetField | null,
  length: number,
) {
  for (let i = 0; i < length; i++) {
    const value = attr(i)
    if (value == null) {
      continue
    }
    addTargetField(index, key, value)
  }
}

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

      // TODO iguridi: do not block page load while building index
      const search_index = new Index({tokenize: 'forward', stemmer: 'false'})
      for (let i = 0; i < build.targetsLength(); i++) {
        let target = build.targets(i)
        const label = target?.configuredTargetLabel()
        if (target == null || label == null) {
          continue
        }
        addIfExists(search_index, label, target.name())
        addIfExists(search_index, label, target.oncall())
        addIfExists(search_index, label, target.executionPlatform())
        addIfExists(search_index, label, target.package_())
        addIfExists(search_index, label, target.targetConfiguration())
        addIfExists(search_index, label, target.type())
        addIfExists(search_index, label, label)
        addListOfStrings(search_index, label, i => target.deps(i), target.depsLength())
        addListOfTargetFields(
          search_index,
          label,
          (i: number) => target.attrs(i),
          target.attrsLength(),
        )
      }

      // This should run just once total
      setData({build, allTargets, rootTarget, search_index})
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
          <TargetView view={TARGET_VIEW} />
          <SearchView view={SEARCH_VIEW} />
        </Router>
      </DataContext.Provider>
    )
  }
}

const container = document.getElementById('root') as HTMLElement
const root = createRoot(container)

root.render(<App />)
