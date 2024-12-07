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
import {Node} from './App'
import {formatTargetLabel} from './formatTargetLabel'

export let indexCache: Index | null = null

export function tokenizer(text: string): string[] {
  return text.split(/[\s-_()\\\/#:]/g).filter(x => x.length > 0)
}

export async function indexEverything(build: Build, graph: Map<number, Node>): Promise<void> {
  // TODO iguridi: make this in a js worker
  const searchIndex = new Index({
    context: false,
    tokenize: tokenizer,
    stemmer: false,
  })
  for (let i = 0; i < build.targetsLength(); i++) {
    if (!graph.has(i)) {
      continue
    }
    let target = build.targets(i)!
    const label = target.label()!
    let data = [
      target.name(),
      target.oncall(),
      target.executionPlatform(),
      target.package_(),
      target.targetConfiguration(),
      target.type(),
      label.targetLabel()!,
      label.cfg()!,
      label.execCfg()!,
    ].filter(x => x != null)

    searchIndex.append(i, data.join(' '))
  }
  indexCache = searchIndex
}
