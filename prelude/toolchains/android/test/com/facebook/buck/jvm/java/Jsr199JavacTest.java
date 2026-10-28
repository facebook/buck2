/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java;

import static com.facebook.buck.jvm.java.JavacLanguageLevelOptions.TARGETED_JAVA_VERSION;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.cd.model.java.AbiGenerationMode;
import com.facebook.buck.cd.model.java.BuildTargetValue.Type;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.core.BuildTargetValue;
import com.facebook.buck.util.ClassLoaderCache;
import com.facebook.buck.util.Verbosity;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSortedSet;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.logging.Level;
import java.util.zip.ZipFile;
import javax.tools.ToolProvider;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class Jsr199JavacTest {
  @Rule public TemporaryFolder temporaryFolder = new TemporaryFolder();
  private static final RelPath PATH_TO_SRCS_LIST = RelPath.get("srcs_list");
  public static final ImmutableSortedSet<RelPath> SOURCE_FILES =
      ImmutableSortedSet.orderedBy(RelPath.comparator()).add(RelPath.get("foobar.java")).build();

  @Test
  public void testJavacCommand() {
    Jsr199Javac.ResolvedJsr199Javac firstOrder = createTestStep();
    Jsr199Javac.ResolvedJsr199Javac warn = createTestStep();
    Jsr199Javac.ResolvedJsr199Javac transitive = createTestStep();

    assertEquals(
        String.format(
            "javac -source %s -target %s -g -d . -classpath foo.jar @%s",
            TARGETED_JAVA_VERSION, TARGETED_JAVA_VERSION, PATH_TO_SRCS_LIST),
        firstOrder.getDescription(getArgs().add("foo.jar").build(), PATH_TO_SRCS_LIST));
    assertEquals(
        String.format(
            "javac -source %s -target %s -g -d . -classpath foo.jar @%s",
            TARGETED_JAVA_VERSION, TARGETED_JAVA_VERSION, PATH_TO_SRCS_LIST),
        warn.getDescription(getArgs().add("foo.jar").build(), PATH_TO_SRCS_LIST));
    assertEquals(
        String.format(
            "javac -source %s -target %s -g -d . -classpath bar.jar%sfoo.jar @%s",
            TARGETED_JAVA_VERSION, TARGETED_JAVA_VERSION, File.pathSeparator, PATH_TO_SRCS_LIST),
        transitive.getDescription(
            getArgs().add("bar.jar" + File.pathSeparator + "foo.jar").build(), PATH_TO_SRCS_LIST));
  }

  @Test
  public void writesSourceMapAfterDiskAndDirectToJarInvocations() throws Exception {
    for (boolean directToJar : ImmutableList.of(false, true)) {
      Path root = temporaryFolder.newFolder().toPath();
      assertEquals(0, compileWithSourceMap(root, directToJar));
      assertEquals(
          Map.of(
              "p/Container.class",
              Set.of("Container.java"),
              "p/Container$Inner.class",
              Set.of("Container.java"),
              "p/Secondary.class",
              Set.of("Container.java")),
          CompilerClassToSourceMap.read(
                  root.resolve("working").resolve(CompilerClassToSourceMap.JAVA_FILE_NAME))
              .getEntries());
      if (directToJar) {
        try (ZipFile archive = new ZipFile(root.resolve("library.jar").toFile())) {
          assertTrue(archive.getEntry("p/Secondary.class") != null);
        }
      } else {
        assertTrue(Files.isRegularFile(root.resolve("classes/p/Secondary.class")));
      }
    }
  }

  @Test
  public void reportsSourceMapWriteFailure() throws Exception {
    for (boolean directToJar : ImmutableList.of(false, true)) {
      Path root = temporaryFolder.newFolder().toPath();
      Path mapPath = root.resolve("working").resolve(CompilerClassToSourceMap.JAVA_FILE_NAME);
      Files.createDirectories(mapPath);
      RuntimeException failure =
          assertThrows(RuntimeException.class, () -> compileWithSourceMap(root, directToJar));
      assertTrue(failure.getCause() instanceof IOException);
      assertEquals(
          "Failed to write compiler class-to-source map: " + mapPath,
          failure.getCause().getMessage());
      assertTrue(failure.getCause().getCause() instanceof IOException);
    }
  }

  private int compileWithSourceMap(Path root, boolean directToJar) throws Exception {
    Path classes = Files.createDirectory(root.resolve("classes"));
    Files.writeString(
        root.resolve("Container.java"),
        "package p; public class Container { class Inner {} } class Secondary {}");
    CompilerOutputPaths paths =
        new CompilerOutputPaths(
            RelPath.get("classes"),
            RelPath.get("."),
            Optional.empty(),
            RelPath.get("annotations"),
            RelPath.get("srcs"),
            RelPath.get("working"),
            Optional.of(RelPath.get("library.jar")),
            RelPath.get("mapping.json"));
    CompilerOutputPathsValue outputs = new CompilerOutputPathsValue("//:test", paths, paths, paths);
    JarParameters jar =
        directToJar
            ? new JarParameters(
                false,
                true,
                RelPath.get("library.jar"),
                ignored -> false,
                ImmutableSortedSet.orderedBy(RelPath.comparator()).build(),
                ImmutableSortedSet.orderedBy(RelPath.comparator()).build(),
                Optional.empty(),
                Optional.empty(),
                Level.OFF)
            : null;
    try (ClassLoaderCache cache = new ClassLoaderCache();
        Jsr199JavacInvocation invocation =
            new Jsr199JavacInvocation(
                ToolProvider::getSystemJavaCompiler,
                new JavacExecutionContext(
                    System.err, cache, Verbosity.SILENT, AbsPath.of(root), RelPath.get("buck-out")),
                new BuildTargetValue(Type.LIBRARY, "//:test"),
                outputs,
                ImmutableList.of("-d", classes.toString(), "-proc:none"),
                JavacPluginParams.EMPTY,
                JavacPluginParams.EMPTY,
                ImmutableSortedSet.orderedBy(RelPath.comparator())
                    .add(RelPath.get("Container.java"))
                    .build(),
                RelPath.get("srcs"),
                false,
                null,
                jar,
                AbiGenerationMode.CLASS,
                AbiGenerationMode.CLASS,
                null)) {
      return invocation.buildClasses();
    }
  }

  private Jsr199Javac.ResolvedJsr199Javac createTestStep() {
    return JdkProvidedInMemoryJavac.createJsr199Javac();
  }

  private ImmutableList.Builder<String> getArgs() {
    return ImmutableList.<String>builder()
        .add(
            "-source",
            TARGETED_JAVA_VERSION,
            "-target",
            TARGETED_JAVA_VERSION,
            "-g",
            "-d",
            ".",
            "-classpath");
  }
}
