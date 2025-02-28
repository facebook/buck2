/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

public final class ClassUsageMerger {

  private ClassUsageMerger() {}

  public static Map<Path, Map<Path, Integer>> mergeClassUsageMaps(
      Map<Path, Map<Path, Integer>> prevClassUsageMap,
      Map<Path, Map<Path, Integer>> currentClassUsageMap) {
    if (prevClassUsageMap.isEmpty()) {
      return currentClassUsageMap;
    }
    if (currentClassUsageMap.isEmpty()) {
      return prevClassUsageMap;
    }

    Map<Path, Map<Path, Integer>> mergedClassUsageMap = new HashMap<>(prevClassUsageMap);

    for (Map.Entry<Path, Map<Path, Integer>> classUsageEntry : currentClassUsageMap.entrySet()) {
      Path jarPath = classUsageEntry.getKey();
      Map<Path, Integer> classPathMap = classUsageEntry.getValue();

      if (!mergedClassUsageMap.containsKey(jarPath)) {
        mergedClassUsageMap.put(jarPath, classPathMap);
        continue;
      }

      Map<Path, Integer> mergedClassPathMap = mergedClassUsageMap.get(jarPath);
      for (Map.Entry<Path, Integer> newClassPathEntry : classPathMap.entrySet()) {
        if (!mergedClassPathMap.containsKey(newClassPathEntry.getKey())) {
          mergedClassPathMap.put(newClassPathEntry.getKey(), newClassPathEntry.getValue());
        }
      }
    }

    return mergedClassUsageMap;
  }
}
