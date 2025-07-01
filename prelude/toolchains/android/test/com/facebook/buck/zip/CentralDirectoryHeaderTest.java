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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.testutil.TemporaryPaths;
import com.facebook.buck.util.zip.CentralDirectoryFileHeader;
import com.facebook.buck.util.zip.CentralDirectoryHeader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.Rule;
import org.junit.Test;

public class CentralDirectoryHeaderTest extends JarTestSupport {

  @Rule public TemporaryPaths tmp = new TemporaryPaths();

  @Test
  public void testCentralDirectoryHeaderOfNonExistentFile() throws IOException {

    final AbsPath outputJar = tmp.getRoot().resolve("nonexistent-file-.jar");
    final CentralDirectoryHeader directory = readCentralDirectoryHeader(outputJar.getPath());

    assertFalse(Files.exists(outputJar.getPath()));
    assertEquals(directory.getOffset(), 0);
    assertEquals(directory.getCount(), 0);
    assertTrue(directory.getFiles().isEmpty());
  }

  @Test
  public void testCentralDirectoryHeaderOfEmptyFile() throws IOException {

    final Path emptyFile = tmp.newFile().getPath();
    final CentralDirectoryHeader directory = readCentralDirectoryHeader(emptyFile);

    assertEquals(0, directory.getOffset());
    assertEquals(0, directory.getCount());
    assertTrue(directory.getFiles().isEmpty());
  }

  @Test
  public void testCentralDirectoryHeaderOfEmptyJar() throws IOException {

    final Path emptyJar = newJar().writeTo(tmp);
    final CentralDirectoryHeader directory = readCentralDirectoryHeader(emptyJar);

    assertTrue(directory.getOffset() > 0);
    assertEquals(1, directory.getCount());
    assertEquals(1, directory.getFiles().size());
    assertEquals(List.of(MANIFEST), readJarEntryNames(emptyJar));
  }

  @Test
  public void testCentralDirectoryHeaderWithFiles() throws IOException {

    final Path outputJar = newJar().entries(ENTRY_1, ENTRY_2, ENTRY_3).writeTo(tmp);
    final CentralDirectoryHeader directory = readCentralDirectoryHeader(outputJar);

    final List<String> expectedEntries = List.of(ENTRY_1, ENTRY_2, ENTRY_3, MANIFEST);
    final List<String> valuesToCheck = List.of(ENTRY_1, ENTRY_2, ENTRY_3);

    final List<String> headerFileNames =
        directory.getFiles().stream().map(CentralDirectoryFileHeader::getName).collect(toList());

    assertTrue(directory.getOffset() > 0);
    assertEquals(expectedEntries.size(), headerFileNames.size());
    assertEquals(expectedEntries.size(), directory.getCount());
    assertEquals(expectedEntries, headerFileNames);
    assertEquals(expectedEntries, readJarEntryNames(outputJar));
    assertEquals(valuesToCheck, readJarEntryValues(outputJar, valuesToCheck));
  }
}
