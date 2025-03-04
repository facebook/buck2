@file:JvmName("ClassUsageMerger")

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package toolchains.android.src.com.facebook.buck.jvm.kotlin.compilerplugins.usedclasses

import java.nio.file.Path

fun mergeClassUsageMaps(
    prevClassUsageMap: Map<Path, Map<Path, Int>>,
    currentClassUsageMap: Map<Path, Map<Path, Int>>
): Map<Path, Map<Path, Int>> {
  if (prevClassUsageMap.isEmpty()) {
    return currentClassUsageMap
  }
  if (currentClassUsageMap.isEmpty()) {
    return prevClassUsageMap
  }

  val mergedClassUsageMap: MutableMap<Path, MutableMap<Path, Int>> =
      HashMap(prevClassUsageMap.mapValues { (_, map) -> map.toMutableMap() })

  for ((jarPath, classPathMap) in currentClassUsageMap) {
    if (!mergedClassUsageMap.containsKey(jarPath)) {
      mergedClassUsageMap[jarPath] = classPathMap.toMutableMap()
      continue
    }

    val mergedClassPathMap = mergedClassUsageMap.getValue(jarPath)
    for ((key, value) in classPathMap) {
      if (!mergedClassPathMap.containsKey(key)) {
        mergedClassPathMap[key] = value
      }
    }
  }

  return mergedClassUsageMap
}
