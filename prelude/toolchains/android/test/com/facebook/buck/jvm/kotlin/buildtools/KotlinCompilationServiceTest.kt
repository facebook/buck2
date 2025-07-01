/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.buildtools

import com.facebook.buck.core.filesystems.AbsPath
import com.facebook.buck.jvm.kotlin.buildtools.snapshot.ClasspathSnapshotGenerator
import com.facebook.buck.jvm.kotlin.buildtools.snapshot.SnapshotGranularity
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.ClasspathChanges
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlinSourceChanges
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode
import com.facebook.buck.testutil.TemporaryPaths
import com.google.common.collect.ImmutableList
import java.io.File
import java.nio.file.Files
import java.util.UUID
import kotlin.io.path.absolute
import kotlin.io.path.extension
import kotlin.io.path.listDirectoryEntries
import kotlin.io.path.toPath
import org.jetbrains.kotlin.buildtools.api.CompilationResult
import org.jetbrains.kotlin.buildtools.api.CompilationService
import org.jetbrains.kotlin.buildtools.api.ExperimentalBuildToolsApi
import org.jetbrains.kotlin.buildtools.api.ProjectId
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotEquals
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Rule
import org.junit.Test
import org.mockito.kotlin.mock

@OptIn(ExperimentalBuildToolsApi::class)
internal class KotlinCompilationServiceTest {

  @JvmField @Rule val temporaryFolder: TemporaryPaths = TemporaryPaths(false)

  private lateinit var kotlinCompilationService: KotlinCompilationService

  private lateinit var sourcesDir: AbsPath
  private lateinit var classesDir: AbsPath
  private lateinit var incrementalDir: AbsPath
  private lateinit var kotlinDepFile: AbsPath
  private lateinit var jvmAbiGenWorkingDir: AbsPath
  private lateinit var fooSourceFile: KotlinSourceFile
  private lateinit var barSourceFile: KotlinSourceFile

  private lateinit var librariesDir: AbsPath
  private lateinit var librarySnapshotsDir: AbsPath

  @Before
  fun setUp() {
    kotlinCompilationService =
        KotlinCompilationService(
            CompilationService.loadImplementation(this::class.java.classLoader), mock())

    sourcesDir = temporaryFolder.newFolder("src")
    classesDir = temporaryFolder.newFolder("__classes__")
    kotlinDepFile = temporaryFolder.newFile("kotlin-used-classes.json")
    jvmAbiGenWorkingDir = temporaryFolder.newFolder("jvm_abi_gen_working_dir")
    incrementalDir = temporaryFolder.newFolder("incrementalState")

    librariesDir = temporaryFolder.newFolder("libraries")
    librarySnapshotsDir = temporaryFolder.newFolder("library_snapshots")

    fooSourceFile =
        KotlinSourceFile(
            sourcesDir.resolve("Foo.kt"),
            """
                |class Foo {
                |
                | fun foo() {
                |   println("foo")
                | }
                |
                | fun bar() {}
                |}
                |"""
                .trimMargin())

    barSourceFile =
        KotlinSourceFile(
            sourcesDir.resolve("Bar.kt"),
            """
                |class Bar(private val foo: Foo) {
                |  fun bar() {
                |    foo.foo()
                |  }
                |}
                |"""
                .trimMargin())
  }

  @Test
  fun `when compile non incrementally, all source files are recompiled`() {
    assertEquals(true, classesDir.path.listDirectoryEntries().isEmpty())

    val result =
        kotlinCompilationService.compile(
            projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
            arguments =
                createCompilerArgs(
                    sourceFiles = listOf(fooSourceFile, barSourceFile), outputDir = classesDir),
            mode = KotlincMode.NonIncremental)

    assertEquals(CompilationResult.COMPILATION_SUCCESS, result)
    val classes = classesDir.path.listDirectoryEntries().filter { it.extension == "class" }
    assertEquals(classes.size, 2)
    assertTrue(classes.any { it.endsWith("Foo.class") })
    assertTrue(classes.any { it.endsWith("Bar.class") })
  }

  @Test
  fun `when compile incrementally, all source files are recompiled on the first run`() {
    assertEquals(true, incrementalDir.path.listDirectoryEntries().isEmpty())

    val result =
        kotlinCompilationService.compile(
            projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
            arguments =
                createCompilerArgs(
                    sourceFiles = listOf(fooSourceFile, barSourceFile), outputDir = classesDir),
            mode = createIncrementalMode())

    assertEquals(CompilationResult.COMPILATION_SUCCESS, result)
    assertTrue(incrementalDir.path.listDirectoryEntries().isNotEmpty())
    val classes = classesDir.path.listDirectoryEntries().filter { it.extension == "class" }
    assertEquals(2, classes.size)
    assertTrue(classes.any { it.endsWith("Foo.class") })
    assertTrue(classes.any { it.endsWith("Bar.class") })
  }

  @Test
  fun `when a non-abi change in Foo, only Foo gets recompiled`() {
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(fooSourceFile, barSourceFile), outputDir = classesDir),
        mode = createIncrementalMode())
    val initialClassTimestamps = getClassModificationTimes()

    fooSourceFile.changeContent("println(\"foo\")", "println(\"foo!\")")
    val result =
        kotlinCompilationService.compile(
            projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
            arguments =
                createCompilerArgs(
                    sourceFiles = listOf(fooSourceFile, barSourceFile), outputDir = classesDir),
            mode = createIncrementalMode())

    assertEquals(CompilationResult.COMPILATION_SUCCESS, result)
    val postCompilationTimestamps = getClassModificationTimes()
    assertNotEquals(initialClassTimestamps["Foo.class"], postCompilationTimestamps["Foo.class"])
    assertEquals(initialClassTimestamps["Bar.class"], postCompilationTimestamps["Bar.class"])
  }

  @Test
  fun `when a abi change in Foo, all affected files gets recompiled`() {
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(fooSourceFile, barSourceFile), outputDir = classesDir),
        mode = createIncrementalMode())
    val initialClassTimestamps = getClassModificationTimes()
    fooSourceFile.changeContent("foo()", "foo(i: Int = 1)")

    val result =
        kotlinCompilationService.compile(
            projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
            arguments =
                createCompilerArgs(
                    sourceFiles = listOf(fooSourceFile, barSourceFile), outputDir = classesDir),
            mode = createIncrementalMode())

    assertEquals(CompilationResult.COMPILATION_SUCCESS, result)
    val postCompilationTimestamps = getClassModificationTimes()
    assertNotEquals(initialClassTimestamps["Foo.class"], postCompilationTimestamps["Foo.class"])
    assertNotEquals(initialClassTimestamps["Bar.class"], postCompilationTimestamps["Bar.class"])
  }

  @Test
  fun `when a compilation error in Foo, its output is restored`() {
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(fooSourceFile, barSourceFile), outputDir = classesDir),
        mode = createIncrementalMode())
    val initialClassTimestamps = getClassModificationTimes()
    fooSourceFile.changeContent("foo()", "foo(i: String = 1)")

    val result =
        kotlinCompilationService.compile(
            projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
            arguments =
                createCompilerArgs(
                    sourceFiles = listOf(fooSourceFile, barSourceFile), outputDir = classesDir),
            mode = createIncrementalMode())

    assertEquals(CompilationResult.COMPILATION_ERROR, result)
    val postCompilationTimestamps = getClassModificationTimes()
    assertEquals(initialClassTimestamps["Bar.class"], postCompilationTimestamps["Bar.class"])
    assertEquals(initialClassTimestamps["Foo.class"], postCompilationTimestamps["Foo.class"])
  }

  @Test
  fun `when a non-abi change in a Foo library, Bar does not get recompiled`() {
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(fooSourceFile),
                outputDir = librariesDir,
            ),
        mode = KotlincMode.NonIncremental)
    var snapshot = generateClasspathSnapshot(librariesDir, SnapshotGranularity.CLASS_MEMBER_LEVEL)
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(barSourceFile),
                outputDir = classesDir,
                classpath = listOf(librariesDir)),
        mode =
            createIncrementalMode(
                ClasspathChanges.ToBeComputedByIncrementalCompiler(ImmutableList.of(snapshot))))
    val initialClassTimestamps = getClassModificationTimes()

    fooSourceFile.changeContent("println(\"foo\")", "println(\"foo!\")")
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(fooSourceFile),
                outputDir = librariesDir,
            ),
        mode = KotlincMode.NonIncremental)
    snapshot = generateClasspathSnapshot(librariesDir, SnapshotGranularity.CLASS_MEMBER_LEVEL)
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(barSourceFile),
                outputDir = classesDir,
                classpath = listOf(librariesDir)),
        mode =
            createIncrementalMode(
                ClasspathChanges.ToBeComputedByIncrementalCompiler(ImmutableList.of(snapshot))))
    val postCompilationTimestamps = getClassModificationTimes()

    assertEquals(initialClassTimestamps["Bar.class"], postCompilationTimestamps["Bar.class"])
  }

  @Test
  fun `when an abi change in a Foo library that impacts Bar, Bar gets recompiled`() {
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(fooSourceFile),
                outputDir = librariesDir,
            ),
        mode = KotlincMode.NonIncremental)
    var snapshot = generateClasspathSnapshot(librariesDir, SnapshotGranularity.CLASS_MEMBER_LEVEL)
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(barSourceFile),
                outputDir = classesDir,
                classpath = listOf(librariesDir)),
        mode =
            createIncrementalMode(
                ClasspathChanges.ToBeComputedByIncrementalCompiler(ImmutableList.of(snapshot))))
    val initialClassTimestamps = getClassModificationTimes()

    fooSourceFile.changeContent("foo()", "foo(i: Int = 1)")
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(fooSourceFile),
                outputDir = librariesDir,
            ),
        mode = KotlincMode.NonIncremental)
    snapshot = generateClasspathSnapshot(librariesDir, SnapshotGranularity.CLASS_MEMBER_LEVEL)
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(barSourceFile),
                outputDir = classesDir,
                classpath = listOf(librariesDir)),
        mode =
            createIncrementalMode(
                ClasspathChanges.ToBeComputedByIncrementalCompiler(ImmutableList.of(snapshot))))
    val postCompilationTimestamps = getClassModificationTimes()

    assertNotEquals(initialClassTimestamps["Bar.class"], postCompilationTimestamps["Bar.class"])
  }

  @Test
  fun `when an abi change in a Foo library that does not impact Bar and CLASS_MEMBER_LEVEL snapshot is used, Bar does not get recompiled`() {
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(fooSourceFile),
                outputDir = librariesDir,
            ),
        mode = KotlincMode.NonIncremental)
    var snapshot = generateClasspathSnapshot(librariesDir, SnapshotGranularity.CLASS_MEMBER_LEVEL)
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(barSourceFile),
                outputDir = classesDir,
                classpath = listOf(librariesDir)),
        mode =
            createIncrementalMode(
                ClasspathChanges.ToBeComputedByIncrementalCompiler(ImmutableList.of(snapshot))))
    val initialClassTimestamps = getClassModificationTimes()

    fooSourceFile.changeContent("fun bar() {}", "")
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(fooSourceFile),
                outputDir = librariesDir,
            ),
        mode = KotlincMode.NonIncremental)
    snapshot = generateClasspathSnapshot(librariesDir, SnapshotGranularity.CLASS_MEMBER_LEVEL)
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(barSourceFile),
                outputDir = classesDir,
                classpath = listOf(librariesDir)),
        mode =
            createIncrementalMode(
                ClasspathChanges.ToBeComputedByIncrementalCompiler(ImmutableList.of(snapshot))))
    val postCompilationTimestamps = getClassModificationTimes()

    assertEquals(initialClassTimestamps["Bar.class"], postCompilationTimestamps["Bar.class"])
  }

  @Test
  fun `when an abi change in a Foo library that does not impact Bar and CLASS_LEVEL snapshot is used, Bar gets recompiled`() {
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(fooSourceFile),
                outputDir = librariesDir,
            ),
        mode = KotlincMode.NonIncremental)
    var snapshot = generateClasspathSnapshot(librariesDir, SnapshotGranularity.CLASS_LEVEL)
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(barSourceFile),
                outputDir = classesDir,
                classpath = listOf(librariesDir)),
        mode =
            createIncrementalMode(
                ClasspathChanges.ToBeComputedByIncrementalCompiler(ImmutableList.of(snapshot))))
    val initialClassTimestamps = getClassModificationTimes()

    fooSourceFile.changeContent("fun bar() {}", "")
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(fooSourceFile),
                outputDir = librariesDir,
            ),
        mode = KotlincMode.NonIncremental)
    snapshot = generateClasspathSnapshot(librariesDir, SnapshotGranularity.CLASS_LEVEL)
    kotlinCompilationService.compile(
        projectId = ProjectId.ProjectUUID(UUID.randomUUID()),
        arguments =
            createCompilerArgs(
                sourceFiles = listOf(barSourceFile),
                outputDir = classesDir,
                classpath = listOf(librariesDir)),
        mode =
            createIncrementalMode(
                ClasspathChanges.ToBeComputedByIncrementalCompiler(ImmutableList.of(snapshot))))
    val postCompilationTimestamps = getClassModificationTimes()

    assertNotEquals(initialClassTimestamps["Bar.class"], postCompilationTimestamps["Bar.class"])
  }

  private fun createCompilerArgs(
      sourceFiles: List<KotlinSourceFile>,
      outputDir: AbsPath,
      classpath: List<AbsPath> = emptyList(),
  ): List<String> {
    val stdlibLocation =
        AbsPath.of(
            KotlinVersion::class
                .java
                .protectionDomain
                .codeSource
                .location
                .toURI()
                .toPath()
                .absolute())

    return buildList {
      add("-no-reflect")
      add("-no-stdlib")
      add("-d")
      add(outputDir.toString())
      add("-cp")
      add((classpath + stdlibLocation).joinToString(":"))
      add("-module-name")
      add(temporaryFolder.root.path.fileName.toString())
      addAll(sourceFiles.map { sourceFile -> sourceFile.absPath.toString() })
    }
  }

  private fun createIncrementalMode(
      classPathChanges: ClasspathChanges = ClasspathChanges.NoChanges(ImmutableList.of())
  ) =
      KotlincMode.Incremental(
          rootProjectDir = temporaryFolder.root,
          buildDir = temporaryFolder.root,
          kotlicWorkingDir = incrementalDir,
          kotlinSourceChanges = KotlinSourceChanges.ToBeCalculated,
          classpathChanges = classPathChanges,
          kotlinClassUsageFile = kotlinDepFile,
          rebuildReason = null)

  private fun getClassModificationTimes(): Map<String, Long> =
      classesDir.path.listDirectoryEntries().associate { path ->
        path.fileName.toString() to Files.getLastModifiedTime(path).toMillis()
      }

  private fun generateClasspathSnapshot(
      dependencyOutput: AbsPath,
      granularity: SnapshotGranularity
  ): File {
    // see details in docs for `CachedClasspathSnapshotSerializer` for details why we can't use a
    // fixed name
    val snapshotFile = librarySnapshotsDir.resolve("dep-${System.currentTimeMillis()}.snapshot")
    ClasspathSnapshotGenerator(dependencyOutput.path, snapshotFile.path, granularity).run()

    return snapshotFile.toFile()
  }
}
