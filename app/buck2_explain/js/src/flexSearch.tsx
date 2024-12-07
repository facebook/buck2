/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

import {Index} from 'flexsearch-ts'
import {Build} from './fbs/explain'
import {formatTargetLabel} from './formatTargetLabel'

export let indexCache: Index | null = null

function addIfExists(index: Index, key: string, element: string | null | undefined) {
  if (element != null) {
    index.append(key, element)
  }
}

export async function indexEverything(build: Build): Promise<void> {
  // TODO iguridi: make this in a js worker
  const searchIndex = new Index({tokenize: 'forward', stemmer: false})
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
  }
  indexCache = searchIndex
}
