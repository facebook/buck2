/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd

import com.facebook.buck.util.json.ObjectMappers
import com.fasterxml.jackson.core.type.TypeReference
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Optional
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class DepFileUtilsTest {

  @get:Rule val tempFolder = TemporaryFolder()

  @Test
  fun `when usedClassesToDepFile is called then it produces sorted output`() {
    val usedClassesMapPath =
        createUsedClassesJson(
            mapOf(
                "/path/to/C.jar" to setOf("Class3.class", "Class1.class"),
                "/path/to/A.jar" to setOf("ClassA.class"),
                "/path/to/B.jar" to setOf("ClassB.class")))
    val depFileOutput = tempFolder.newFile("dep-file.txt").toPath()

    DepFileUtils.usedClassesToDepFile(listOf(usedClassesMapPath), depFileOutput, Optional.empty())

    val outputLines = Files.readAllLines(depFileOutput)
    val expectedSortedPaths = listOf("/path/to/A.jar", "/path/to/B.jar", "/path/to/C.jar")
    assertEquals(expectedSortedPaths, outputLines)
  }

  @Test
  fun `when usedClassesToDepFile is called with jarToJarDirMap then it produces sorted output`() {
    val usedClassesMapPath =
        createUsedClassesJson(
            mapOf(
                "/path/to/C.jar" to setOf("Class3.class", "Class1.class"),
                "/path/to/A.jar" to setOf("ClassZ.class"),
                "/path/to/B.jar" to setOf("ClassB.class")))
    val jarToJarDirMapPath = tempFolder.newFile("jar-to-dir-map.txt").toPath()
    Files.write(
        jarToJarDirMapPath, listOf("/path/to/C.jar /expanded/C", "/path/to/A.jar /expanded/A"))
    val depFileOutput = tempFolder.newFile("dep-file.txt").toPath()

    DepFileUtils.usedClassesToDepFile(
        listOf(usedClassesMapPath), depFileOutput, Optional.of(jarToJarDirMapPath))

    val outputLines = Files.readAllLines(depFileOutput)
    val expectedSortedPaths =
        listOf(
            "/expanded/A/ClassZ.class",
            "/expanded/C/Class1.class",
            "/expanded/C/Class3.class",
            "/path/to/B.jar")
    assertEquals(expectedSortedPaths, outputLines)
  }

  @Test
  fun `when usedClassesToUsedJars is called then it produces sorted output`() {
    val usedClassesMapPath =
        createUsedClassesJson(
            mapOf(
                "/path/to/C.jar" to setOf("Class3.class", "Class1.class"),
                "/path/to/A.jar" to setOf("ClassA.class"),
                "/path/to/B.jar" to setOf("ClassB.class")))
    val usedJarsOutput = tempFolder.newFile("used-jars.txt").toPath()

    DepFileUtils.usedClassesToUsedJars(listOf(usedClassesMapPath), usedJarsOutput)

    val outputLines = Files.readAllLines(usedJarsOutput).joinToString("\n")
    val actual =
        ObjectMappers.readValue(
            outputLines, object : TypeReference<LinkedHashMap<String, List<String>>>() {})
    val expected =
        mapOf(
            "/path/to/A.jar" to listOf("ClassA.class"),
            "/path/to/B.jar" to listOf("ClassB.class"),
            "/path/to/C.jar" to listOf("Class1.class", "Class3.class"),
        )
    assertEquals(expected, actual)
  }

  private fun createUsedClassesJson(classesMap: Map<String, Set<String>>): Path {
    val file = tempFolder.newFile("used-classes.json")
    val mappedInput =
        classesMap.entries.associate { (jarPath, classes) ->
          Paths.get(jarPath) to classes.map { Paths.get(it) }.toSet()
        }

    ObjectMappers.WRITER.writeValue(file, mappedInput)
    return file.toPath()
  }
}
