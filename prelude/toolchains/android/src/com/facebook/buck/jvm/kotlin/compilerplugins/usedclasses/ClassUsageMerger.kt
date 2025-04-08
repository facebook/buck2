@file:JvmName("ClassUsageMerger")

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.compilerplugins.usedclasses

import java.nio.file.Path

fun mergeClassUsageMaps(
    prevClassUsages: Map<Path, Set<Path>>,
    currentClassUsages: Map<Path, Set<Path>>
): Map<Path, Set<Path>> {
  if (prevClassUsages.isEmpty()) {
    return currentClassUsages
  }
  if (currentClassUsages.isEmpty()) {
    return prevClassUsages
  }

  val mergedClassUsageMap: MutableMap<Path, MutableSet<Path>> =
      HashMap(prevClassUsages.mapValues { (_, set) -> set.toMutableSet() })

  for ((jarPath, classPathMap) in currentClassUsages) {
    if (!mergedClassUsageMap.containsKey(jarPath)) {
      mergedClassUsageMap[jarPath] = classPathMap.toMutableSet()
      continue
    }

    val mergedClassPathEntry = mergedClassUsageMap.getValue(jarPath)
    mergedClassPathEntry.addAll(classPathMap)
  }

  return mergedClassUsageMap
}
