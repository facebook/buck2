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
}
