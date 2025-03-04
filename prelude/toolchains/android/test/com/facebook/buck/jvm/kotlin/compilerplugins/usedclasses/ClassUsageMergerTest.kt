/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package toolchains.android.test.com.facebook.buck.jvm.kotlin.compilerplugins.usedclasses

import java.nio.file.Path
import java.nio.file.Paths
import java.util.HashMap
import org.junit.Assert.assertEquals
import org.junit.Test
import toolchains.android.src.com.facebook.buck.jvm.kotlin.compilerplugins.usedclasses.mergeClassUsageMaps

internal class ClassUsageMergerTest {

  @Test
  fun `when previous class usage is empty then current class usage is returned`() {
    val prevClassUsageMap: Map<Path, MutableMap<Path, Int>> = HashMap()
    val currentClassUsageMap: MutableMap<Path, MutableMap<Path, Int>> = HashMap()
    currentClassUsageMap[Paths.get("A.jar")] = HashMap()

    val mergedMap: Map<Path, Map<Path, Int>> =
        mergeClassUsageMaps(prevClassUsageMap, currentClassUsageMap)

    assertEquals(currentClassUsageMap, mergedMap)
  }

  @Test
  fun `when current class usage is empty then previous class usage is returned`() {
    val prevClassUsageMap: MutableMap<Path, Map<Path, Int>> = HashMap()
    prevClassUsageMap[Paths.get("A.jar")] = HashMap()
    val currentClassUsageMap: Map<Path, Map<Path, Int>> = HashMap()

    val mergedMap: Map<Path, Map<Path, Int>> =
        mergeClassUsageMaps(prevClassUsageMap, currentClassUsageMap)

    assertEquals(prevClassUsageMap, mergedMap)
  }

  @Test
  fun `when outer addition is made then new class usage is included in merged map`() {
    val prevClassUsageMap: Map<Path, MutableMap<Path, Int>> = classUsageMapBase
    val currentClassUsageMap = classUsageMapBase
    val innerMap: MutableMap<Path, Int> = HashMap()
    innerMap[Paths.get("C.class")] = 1
    currentClassUsageMap[Paths.get("new-class-abi.jar")] = innerMap

    val mergedMap: Map<Path, Map<Path, Int>> =
        mergeClassUsageMaps(prevClassUsageMap, currentClassUsageMap)

    assertEquals(currentClassUsageMap, mergedMap)
  }

  @Test
  fun `when inner addition is made then new class is included in merged inner map`() {
    val prevClassUsageMap: Map<Path, MutableMap<Path, Int>> = classUsageMapBase
    val currentClassUsageMap: Map<Path, MutableMap<Path, Int>> = classUsageMapBase
    currentClassUsageMap.getValue(Paths.get("class-abi.jar"))[Paths.get("C.class")] = 1

    val mergedMap: Map<Path, Map<Path, Int>> =
        mergeClassUsageMaps(prevClassUsageMap, currentClassUsageMap)

    assertEquals(currentClassUsageMap, mergedMap)
  }

  @Test
  fun `when outer deletion is made then deleted class usage is still included in merged map`() {
    val prevClassUsageMap: Map<Path, MutableMap<Path, Int>> = classUsageMapBase
    val currentClassUsageMap = classUsageMapBase
    currentClassUsageMap.remove(Paths.get("class-abi.jar"))

    val mergedMap: Map<Path, Map<Path, Int>> =
        mergeClassUsageMaps(prevClassUsageMap, currentClassUsageMap)

    assertEquals(prevClassUsageMap, mergedMap)
  }

  @Test
  fun `when inner deletion is made then deleted class is still included in merged inner map`() {
    val prevClassUsageMap: Map<Path, MutableMap<Path, Int>> = classUsageMapBase
    val currentClassUsageMap: Map<Path, MutableMap<Path, Int>> = classUsageMapBase
    currentClassUsageMap
        .getValue(Paths.get("symlink_kotlin-stdlib-2.0.0.jar-class-abi.jar"))
        .remove(Paths.get("kotlin/Unit.class"))

    val mergedMap: Map<Path, Map<Path, Int>> =
        mergeClassUsageMaps(prevClassUsageMap, currentClassUsageMap)

    assertEquals(prevClassUsageMap, mergedMap)
  }

  @Test
  fun `when no changes are made then merged map is equal to both previous and current class usage maps`() {
    val prevClassUsageMap: Map<Path, MutableMap<Path, Int>> = classUsageMapBase
    val currentClassUsageMap: Map<Path, MutableMap<Path, Int>> = classUsageMapBase

    val mergedMap: Map<Path, Map<Path, Int>> =
        mergeClassUsageMaps(prevClassUsageMap, currentClassUsageMap)

    assertEquals(prevClassUsageMap, mergedMap)
    assertEquals(currentClassUsageMap, mergedMap)
  }

  private val classUsageMapBase: MutableMap<Path, MutableMap<Path, Int>>
    get() {
      val outerMap: MutableMap<Path, MutableMap<Path, Int>> = HashMap()
      val innerMap1: MutableMap<Path, Int> = HashMap()
      innerMap1[Paths.get("kotlin/Unit.class")] = 1
      innerMap1[Paths.get("kotlin/io/ConsoleKt.class")] = 1
      innerMap1[Paths.get("kotlin/internal/InlineOnly.class")] = 1
      innerMap1[Paths.get("kotlin/annotation/Target.class")] = 1
      innerMap1[Paths.get("kotlin/annotation/MustBeDocumented.class")] = 1
      innerMap1[Paths.get("kotlin/annotation/Retention.class")] = 1
      outerMap[Paths.get("symlink_kotlin-stdlib-2.0.0.jar-class-abi.jar")] = innerMap1

      val innerMap2: MutableMap<Path, Int> = HashMap()
      innerMap2[Paths.get("java/lang/Object.class")] = 1
      innerMap2[Paths.get("java/lang/annotation/Annotation.class")] = 1
      outerMap[Paths.get("android.jar")] = innerMap2

      val innerMap3: MutableMap<Path, Int> = HashMap()
      innerMap3[Paths.get("D.class")] = 1
      outerMap[Paths.get("class-abi.jar")] = innerMap3

      return HashMap(outerMap)
    }
}
