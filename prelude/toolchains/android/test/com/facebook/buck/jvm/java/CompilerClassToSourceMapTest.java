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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class CompilerClassToSourceMapTest {
  @Rule public TemporaryFolder temporaryFolder = new TemporaryFolder();

  @Test
  public void preservesMultipleSourcesAndUnknownOriginsWithoutAbsolutePaths() throws Exception {
    Path root = temporaryFolder.getRoot().toPath();
    CompilerClassToSourceMap mapping = new CompilerClassToSourceMap();
    mapping.put("p/Facade.class", List.of(root.resolve("B.kt"), root.resolve("A.kt")), root);
    mapping.put("p/Generated.class", List.of(root.getParent().resolve("Outside.java")), root);
    Path output = root.resolve("mapping.json");
    mapping.write(output);
    assertEquals(
        "{\"p/Facade.class\":[\"A.kt\",\"B.kt\"],\"p/Generated.class\":[]}",
        Files.readString(output));
    assertEquals(mapping.getEntries(), CompilerClassToSourceMap.read(output).getEntries());
  }

  @Test
  public void rejectsNullShapesAsMalformedInput() throws Exception {
    Path input = temporaryFolder.newFile("mapping.json").toPath();
    for (String json : List.of("null", "{\"A.class\":null}", "{\"A.class\":[null]}")) {
      Files.writeString(input, json);
      assertThrows(json, IOException.class, () -> CompilerClassToSourceMap.read(input));
    }
  }

  @Test
  public void replacesRecompiledOriginsAndPrunesDeletedOutputs() throws Exception {
    Path root = temporaryFolder.getRoot().toPath();
    Path classes = temporaryFolder.newFolder("classes").toPath();
    Files.createFile(classes.resolve("Kept.class"));
    CompilerClassToSourceMap mapping = new CompilerClassToSourceMap();
    mapping.put("Kept.class", List.of(root.resolve("Old.kt")), root);
    mapping.put("Removed.class", List.of(root.resolve("Old.kt")), root);
    mapping.put("Kept.class", List.of(root.resolve("New.kt")), root);
    mapping.retainExistingOutputs(classes);
    assertEquals(Map.of("Kept.class", Set.of("New.kt")), mapping.getEntries());
  }

  @Test
  public void recordsActualJavacOutputsWithDiskAndJarFileManagers() throws Exception {
    Path root = temporaryFolder.getRoot().toPath();
    Path source = root.resolve("Container.java");
    Files.writeString(
        source,
        "package p; public class Container { class Inner {} Object f() { class Local {} "
            + "return new Runnable() { public void run() {} }; } } class Secondary {}");
    for (boolean directToJar : List.of(false, true)) {
      CompilerClassToSourceMap mapping = new CompilerClassToSourceMap();
      var compiler = ToolProvider.getSystemJavaCompiler();
      try (StandardJavaFileManager standard = compiler.getStandardFileManager(null, null, null)) {
        StandardJavaFileManager output =
            directToJar
                ? new JavaInMemoryFileManager(
                    standard, root.resolve("library.jar"), ignored -> false)
                : standard;
        ListenableFileManager manager = new ListenableFileManager(output, mapping, root);
        assertEquals(
            true,
            compiler
                .getTask(
                    null,
                    manager,
                    null,
                    List.of("-d", root.toString(), "-proc:none"),
                    null,
                    standard.getJavaFileObjects(source.toFile()))
                .call());
      }
      assertEquals(
          Map.of(
              "p/Container.class", Set.of("Container.java"),
              "p/Container$Inner.class", Set.of("Container.java"),
              "p/Container$1Local.class", Set.of("Container.java"),
              "p/Container$1.class", Set.of("Container.java"),
              "p/Secondary.class", Set.of("Container.java")),
          mapping.getEntries());
    }
  }
}
