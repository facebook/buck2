/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import {Index} from 'flexsearch-ts'
import {Build, TargetField, TargetValue, TargetValueType} from './fbs/explain'
import {formatTargetLabel} from './formatTargetLabel'

export let indexCache: Index | null = null

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

export async function indexEverything(build: Build): Promise<void> {
  // TODO iguridi: make this in a js worker
  const searchIndex = new Index({tokenize: 'forward', stemmer: 'false'})
  for (let i = 0; i < build.targetsLength(); i++) {
    let target = build.targets(i)!
    const label = target.label()!
    let identifier = formatTargetLabel(label)
    addIfExists(searchIndex, identifier, target.name())
    addIfExists(searchIndex, identifier, target.oncall())
    addIfExists(searchIndex, identifier, target.executionPlatform())
    addIfExists(searchIndex, identifier, target.package_())
    addIfExists(searchIndex, identifier, target.targetConfiguration())
    addIfExists(searchIndex, identifier, target.type())
    addIfExists(searchIndex, identifier, label.targetLabel()!)
    addIfExists(searchIndex, identifier, label.cfg()!)
    addIfExists(searchIndex, identifier, label.execCfg()!)
    addListOfStrings(
      searchIndex,
      identifier,
      i => formatTargetLabel(target.deps(i)!),
      target.depsLength(),
    )
    addListOfTargetFields(
      searchIndex,
      identifier,
      (i: number) => target.attrs(i),
      target.attrsLength(),
    )
  }
  indexCache = searchIndex
}
