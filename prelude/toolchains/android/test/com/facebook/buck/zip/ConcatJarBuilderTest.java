/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.zip;

import static java.util.stream.Collectors.toList;
import static org.junit.Assert.*;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.testutil.TemporaryPaths;
import com.facebook.buck.util.zip.CentralDirectoryHeader;
import com.facebook.buck.util.zip.ConcatJarBuilder;
import com.facebook.buck.util.zip.JarBuilder;
import java.io.IOException;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.jar.Manifest;
import org.junit.Rule;
import org.junit.Test;

public class ConcatJarBuilderTest extends JarTestSupport {

  public static class EmptyClass {}

  @Rule public TemporaryPaths tmp = new TemporaryPaths();

  @Test
  public void testBasicConcatenatedJar() throws IOException {

    final Path firstJar = newJar().entries(ENTRY_1).writeTo(tmp);
    final Path secondJar = newJar().entries(ENTRY_2).writeTo(tmp);

    // keep all zip entries, however the JarFile will only use load only the last entry of same
    // name.
    final Set<String> expectedEntries = Set.of(ENTRY_1, ENTRY_2, MANIFEST, MANIFEST_DIR);
    final Set<String> valuesToCheck = Set.of(ENTRY_1, ENTRY_2);

    final Path outputJar =
        createJarWithConcatJarBuilder(List.of(firstJar, secondJar), tmp.newFile().getPath());

    final CentralDirectoryHeader directory = readCentralDirectoryHeader(outputJar);

    assertTrue(directory.getOffset() > 0);
    assertEquals(expectedEntries, Set.copyOf(readJarEntryNames(outputJar)));
    assertEquals(valuesToCheck, Set.copyOf(readJarValuesAsMap(outputJar, valuesToCheck).values()));
  }

  @Test
  public void testConcatenatedJarWithAppendJar() throws IOException, ClassNotFoundException {

    final Path appendJar = newJar().entry(ENTRY_1).writeTo(tmp);
    final Path firstJar = newJar().entry(ENTRY_2).writeTo(tmp);
    final Path secondJar = newJar().entries(ENTRY_3).writeTo(tmp);
    final Path outputJar = tmp.newFile().getPath();

    // preserve all entries
    final Set<String> expectedContent = Set.of(ENTRY_1, ENTRY_2, ENTRY_3);

    getConcatJarBuilder(List.of(firstJar, secondJar))
        .setAppendJar(appendJar)
        .createJarFile(outputJar);

    final CentralDirectoryHeader directory = readCentralDirectoryHeader(outputJar);

    assertTrue(directory.getOffset() > 0);
    assertEquals(
        expectedContent, Set.copyOf(readJarValuesAsMap(outputJar, expectedContent).values()));
  }

  @Test
  public void testConcatenatedJarWithDuplicatedFiles() throws IOException {

    final Path firstJar = newJar().entries(ENTRY_1, ENTRY_2).writeTo(tmp);
    final Path secondJar = newJar().entries(ENTRY_2, ENTRY_3).writeTo(tmp);

    // preserve all entries
    final Set<String> expectedEntries = Set.of(ENTRY_1, ENTRY_2, ENTRY_3, MANIFEST_DIR, MANIFEST);
    final Set<String> valuesToCheck = Set.of(ENTRY_1, ENTRY_2, ENTRY_3);

    final Path outputJar =
        createJarWithConcatJarBuilder(List.of(firstJar, secondJar), tmp.newFile().getPath());

    final CentralDirectoryHeader directory = readCentralDirectoryHeader(outputJar);

    assertTrue(directory.getOffset() > 0);
    assertEquals(expectedEntries, Set.copyOf(readJarEntryNames(outputJar)));
    assertEquals(valuesToCheck, Set.copyOf(readJarValuesAsMap(outputJar, valuesToCheck).values()));
  }

  @Test
  public void testConcatenatedJarWithMergedManifest() throws IOException, ClassNotFoundException {

    final String firstAttribute = "attribute1";
    final String secondAttribute = "attribute2";

    final Path firstJar = newJar().attr(firstAttribute).writeTo(tmp);
    final Path secondJar = newJar().attr(secondAttribute).writeTo(tmp);

    final Path outputJar =
        createJarWithConcatJarBuilder(List.of(firstJar, secondJar), tmp.newFile().getPath());

    try (URLClassLoader classLoader = loadJarAsTestClassLoader(outputJar)) {
      // check merged manifest
      final Manifest jarManifest = getResourceAsManifest(classLoader, MANIFEST);
      assertEquals(firstAttribute, jarManifest.getMainAttributes().getValue(firstAttribute));
      assertEquals(secondAttribute, jarManifest.getMainAttributes().getValue(secondAttribute));
    }
  }

  @Test
  public void testConcatenatedJarWithClassOverride() throws IOException, ClassNotFoundException {

    final String className = EmptyClass.class.getName();
    final String classResource = className.replace('.', '/') + ".class";
    final byte[] classBytes = getResourceAsBytes(getClass().getClassLoader(), classResource);

    final Path firstJar = newJar().entry(classResource, classBytes).writeTo(tmp);
    final Path secondJar = newJar().entry(classResource, "invalid class bytes").writeTo(tmp);

    final Path outputJar =
        createJarWithConcatJarBuilder(List.of(firstJar, secondJar), tmp.newFile().getPath());

    assertArrayEquals(classBytes, readEntryBytes(outputJar, classResource));

    try (URLClassLoader classLoader = loadJarAsTestClassLoader(outputJar)) {

      assertArrayEquals(classBytes, getResourceAsBytes(classLoader, classResource));

      // Load class from jar
      final Class<?> loadedClass = classLoader.loadClass(className);
      assertNotNull(loadedClass);
      assertNotNull(className, loadedClass.getName());
    }
  }

  Path createJarWithConcatJarBuilder(final List<Path> jars, final Path outputJar)
      throws IOException {
    getConcatJarBuilder(jars).createJarFile(outputJar.toAbsolutePath());
    return outputJar;
  }

  JarBuilder getConcatJarBuilder(List<Path> jars) {
    return new ConcatJarBuilder()
        .setEntriesToJar(jars.stream().map(AbsPath::of).collect(toList()))
        .setShouldMergeManifests(true);
  }
}
