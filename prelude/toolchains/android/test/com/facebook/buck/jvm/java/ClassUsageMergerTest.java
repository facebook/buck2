/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import static org.junit.Assert.assertEquals;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;
import toolchains.android.src.com.facebook.buck.jvm.kotlin.compilerplugins.usedclasses.ClassUsageMerger;

public class ClassUsageMergerTest {

  @Test
  public void testEmptyPreviousClassUsage() {
    Map<Path, Map<Path, Integer>> prevClassUsageMap = new HashMap<>();
    Map<Path, Map<Path, Integer>> currentClassUsageMap = new HashMap<>();
    currentClassUsageMap.put(Paths.get("A.jar"), new HashMap<>());

    Map<Path, Map<Path, Integer>> mergedMap =
        ClassUsageMerger.mergeClassUsageMaps(prevClassUsageMap, currentClassUsageMap);

    assertEquals(currentClassUsageMap, mergedMap);
  }

  @Test
  public void testEmptyCurrentClassUsage() {
    Map<Path, Map<Path, Integer>> prevClassUsageMap = new HashMap<>();
    prevClassUsageMap.put(Paths.get("A.jar"), new HashMap<>());
    Map<Path, Map<Path, Integer>> currentClassUsageMap = new HashMap<>();

    Map<Path, Map<Path, Integer>> mergedMap =
        ClassUsageMerger.mergeClassUsageMaps(prevClassUsageMap, currentClassUsageMap);

    assertEquals(prevClassUsageMap, mergedMap);
  }

  @Test
  public void testMergeClassUsageWhenOuterAddition() {
    Map<Path, Map<Path, Integer>> prevClassUsageMap = getClassUsageMapBase();
    Map<Path, Map<Path, Integer>> currentClassUsageMap = getClassUsageMapBase();
    Map<Path, Integer> innerMap = new HashMap<>();
    innerMap.put(Paths.get("C.class"), 1);
    currentClassUsageMap.put(Paths.get("new-class-abi.jar"), innerMap);

    Map<Path, Map<Path, Integer>> mergedMap =
        ClassUsageMerger.mergeClassUsageMaps(prevClassUsageMap, currentClassUsageMap);

    assertEquals(currentClassUsageMap, mergedMap);
  }

  @Test
  public void testMergeClassUsageWhenInnerAddition() {
    Map<Path, Map<Path, Integer>> prevClassUsageMap = getClassUsageMapBase();
    Map<Path, Map<Path, Integer>> currentClassUsageMap = getClassUsageMapBase();
    currentClassUsageMap.get(Paths.get("class-abi.jar")).put(Paths.get("C.class"), 1);

    Map<Path, Map<Path, Integer>> mergedMap =
        ClassUsageMerger.mergeClassUsageMaps(prevClassUsageMap, currentClassUsageMap);

    assertEquals(currentClassUsageMap, mergedMap);
  }

  @Test
  public void testMergeClassUsageWhenOuterDeletion() {
    Map<Path, Map<Path, Integer>> prevClassUsageMap = getClassUsageMapBase();
    Map<Path, Map<Path, Integer>> currentClassUsageMap = getClassUsageMapBase();
    currentClassUsageMap.remove(Paths.get("class-abi.jar"));

    Map<Path, Map<Path, Integer>> mergedMap =
        ClassUsageMerger.mergeClassUsageMaps(prevClassUsageMap, currentClassUsageMap);

    assertEquals(prevClassUsageMap, mergedMap);
  }

  @Test
  public void testMergeClassUsageWhenInnerDeletion() {
    Map<Path, Map<Path, Integer>> prevClassUsageMap = getClassUsageMapBase();
    Map<Path, Map<Path, Integer>> currentClassUsageMap = getClassUsageMapBase();
    currentClassUsageMap
        .get(Paths.get("symlink_kotlin-stdlib-2.0.0.jar-class-abi.jar"))
        .remove(Paths.get("kotlin/Unit.class"));

    Map<Path, Map<Path, Integer>> mergedMap =
        ClassUsageMerger.mergeClassUsageMaps(prevClassUsageMap, currentClassUsageMap);

    assertEquals(prevClassUsageMap, mergedMap);
  }

  @Test
  public void testMergeClassUsageWhenNoChange() {
    Map<Path, Map<Path, Integer>> prevClassUsageMap = getClassUsageMapBase();
    Map<Path, Map<Path, Integer>> currentClassUsageMap = getClassUsageMapBase();

    Map<Path, Map<Path, Integer>> mergedMap =
        ClassUsageMerger.mergeClassUsageMaps(prevClassUsageMap, currentClassUsageMap);

    assertEquals(prevClassUsageMap, mergedMap);
    assertEquals(currentClassUsageMap, mergedMap);
  }

  private Map<Path, Map<Path, Integer>> getClassUsageMapBase() {
    Map<Path, Map<Path, Integer>> outerMap = new HashMap<>();
    Map<Path, Integer> innerMap1 = new HashMap<>();
    innerMap1.put(Paths.get("kotlin/Unit.class"), 1);
    innerMap1.put(Paths.get("kotlin/io/ConsoleKt.class"), 1);
    innerMap1.put(Paths.get("kotlin/internal/InlineOnly.class"), 1);
    innerMap1.put(Paths.get("kotlin/annotation/Target.class"), 1);
    innerMap1.put(Paths.get("kotlin/annotation/MustBeDocumented.class"), 1);
    innerMap1.put(Paths.get("kotlin/annotation/Retention.class"), 1);
    outerMap.put(Paths.get("symlink_kotlin-stdlib-2.0.0.jar-class-abi.jar"), innerMap1);

    Map<Path, Integer> innerMap2 = new HashMap<>();
    innerMap2.put(Paths.get("java/lang/Object.class"), 1);
    innerMap2.put(Paths.get("java/lang/annotation/Annotation.class"), 1);
    outerMap.put(Paths.get("android.jar"), innerMap2);

    Map<Path, Integer> innerMap3 = new HashMap<>();
    innerMap3.put(Paths.get("D.class"), 1);
    outerMap.put(Paths.get("class-abi.jar"), innerMap3);

    return new HashMap<>(outerMap);
  }
}
