/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import React, {useContext, useState} from 'react'
import {DataContext} from './App'
import {Link, RouterContext, TARGET_VIEW} from './Router'
import {Index, IndexSearchResult} from 'flexsearch-ts'
import {Build, TargetField, TargetValue, TargetValueType} from './fbs/explain'

let indexCache: Index | null = null

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

async function indexEverything(build: Build): Promise<void> {
  // TODO iguridi: make this in a js worker
  const searchIndex = new Index({tokenize: 'forward', stemmer: 'false'})
  for (let i = 0; i < build.targetsLength(); i++) {
    let target = build.targets(i)
    const label = target?.configuredTargetLabel()
    if (target == null || label == null) {
      continue
    }
    addIfExists(searchIndex, label, target.name())
    addIfExists(searchIndex, label, target.oncall())
    addIfExists(searchIndex, label, target.executionPlatform())
    addIfExists(searchIndex, label, target.package_())
    addIfExists(searchIndex, label, target.targetConfiguration())
    addIfExists(searchIndex, label, target.type())
    addIfExists(searchIndex, label, label)
    addListOfStrings(searchIndex, label, i => target.deps(i), target.depsLength())
    addListOfTargetFields(searchIndex, label, (i: number) => target.attrs(i), target.attrsLength())
  }
  indexCache = searchIndex
}

function Checkbox(props: {
  checked: boolean
  onChange: (event: {target: {checked: boolean | ((prevState: boolean) => boolean)}}) => void
}) {
  const {checked, onChange} = props
  return (
    <div className="checkbox">
      <input type="checkbox" checked={checked} onChange={onChange} />
      <label>
        Search everywhere (this may make the page unresponsive for a while, but eventually it
        finishes)
      </label>
    </div>
  )
}

export function SearchView(props: {view: string}) {
  const {build, allTargets} = useContext(DataContext)
  const {params} = useContext(RouterContext)

  const [universalSearch, setUniversalSearch] = useState(false)
  function handleChange(event: {target: {checked: boolean | ((prevState: boolean) => boolean)}}) {
    setUniversalSearch(event.target.checked)
    if (build != null) {
      if (indexCache) {
        return
      } else {
        indexEverything(build)
      }
    }
  }

  const urlParams = new URLSearchParams(params)
  const search = urlParams.get(props.view)

  if (search == null || search.length < 3) {
    return <p>Invalid search "{search}", try again</p>
  }

  let res = null
  if (universalSearch) {
    res = indexCache?.search(search)
  } else {
    res = []
    for (let k of Object.keys(allTargets)) {
      if (k.includes(search)) {
        res.push(k)
      }
    }
  }

  if (res == null || res.length == 0) {
    return (
      <>
        <Checkbox checked={universalSearch} onChange={handleChange} />
        <p>No results for search</p>
      </>
    )
  }

  // Not sure where the dups are coming from, but we want to dedup to prevent
  // undefined behavior in React
  const deduped = dedupeArray(res.map(v => v.toString()))

  return (
    <>
      <Checkbox checked={universalSearch} onChange={handleChange} />
      <h2>Showing targets labels containing "{search}"</h2>
      <ul>
        {deduped.map(label => (
          <li key={label}>
            <Link to={new Map().set(TARGET_VIEW, label)}>{label}</Link>
          </li>
        ))}
      </ul>
    </>
  )
}

function dedupeArray(res: string[]): string[] {
  const array = res.map((value, index) => [value, index])
  const deduped = Array.from(
    new Map(array as Iterable<readonly [unknown, unknown]>).keys(),
  ) as string[]
  return deduped
}
