/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util.zip.collect;

import static org.junit.Assert.assertEquals;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.testutil.TemporaryPaths;
import com.facebook.buck.testutil.integration.TestDataHelper;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.ImmutableSet;
import com.google.common.io.ByteStreams;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

public class ZipEntrySourceCollectionWriterTest {
  @Rule public TemporaryPaths tmp = new TemporaryPaths();

  private Path testDataDir;

  @Before
  public void setUp() {
    testDataDir = TestDataHelper.getTestDataScenario(this, "zip-collector").toAbsolutePath();
  }

  @Test
  public void collectionWithDuplicatesWrittenToZip() throws IOException {
    ZipEntrySourceCollectionBuilder builder =
        new ZipEntrySourceCollectionBuilder(ImmutableSet.of(), OnDuplicateEntry.APPEND);

    builder.addFile("tree.txt", testDataDir.resolve("tree.txt"));
    builder.addZipFile(testDataDir.resolve("zip-1.zip"));
    builder.addZipFile(testDataDir.resolve("zip-2.zip"));

    ZipEntrySourceCollection collection = builder.build();

    ZipEntrySourceCollectionWriter writer =
        new ZipEntrySourceCollectionWriter(tmp.getRoot(), /* hardcodePermissions */ false);

    AbsPath output = tmp.newFolder("output").resolve("output.zip");
    writer.copyToZip(collection, output.getPath());

    ImmutableMap<String, Collection<String>> entries = readZipEntryContent(output).asMap();

    assertEquals(ImmutableMap.of("tree.txt", Arrays.asList("file", "zip-1", "zip-2")), entries);
  }

  @Test
  public void collectionWithoutDuplicatesWrittenToZip() throws IOException {
    ZipEntrySourceCollectionBuilder builder =
        new ZipEntrySourceCollectionBuilder(ImmutableSet.of(), OnDuplicateEntry.OVERWRITE);

    builder.addFile("tree.txt", testDataDir.resolve("tree.txt"));
    builder.addZipFile(testDataDir.resolve("zip-1.zip"));
    builder.addZipFile(testDataDir.resolve("zip-2.zip"));

    ZipEntrySourceCollection collection = builder.build();

    ZipEntrySourceCollectionWriter writer =
        new ZipEntrySourceCollectionWriter(tmp.getRoot(), /* hardcodePermissions */ false);

    AbsPath output = tmp.newFolder("output").resolve("output.zip");
    writer.copyToZip(collection, output.getPath());

    ImmutableMap<String, Collection<String>> entries = readZipEntryContent(output).asMap();

    assertEquals(ImmutableMap.of("tree.txt", Collections.singletonList("zip-2")), entries);
  }

  @Test
  public void directoryEntriesWrittenToZip() throws IOException {
    ZipEntrySourceCollectionBuilder builder =
        new ZipEntrySourceCollectionBuilder(ImmutableSet.of(), OnDuplicateEntry.OVERWRITE);

    builder.addFile("dir/tree.txt", testDataDir.resolve("tree.txt"));

    ZipEntrySourceCollection collection = builder.build();

    ZipEntrySourceCollectionWriter writer =
        new ZipEntrySourceCollectionWriter(tmp.getRoot(), /* hardcodePermissions */ false);

    AbsPath output = tmp.newFolder("output").resolve("output.zip");
    writer.copyToZip(collection, output.getPath());

    ImmutableMap<String, Collection<String>> entries = readZipEntryContent(output).asMap();

    assertEquals(
        ImmutableMap.of(
            "dir/", Collections.singletonList(""),
            "dir/tree.txt", Collections.singletonList("file")),
        entries);
  }

  private ImmutableMultimap<String, String> readZipEntryContent(AbsPath output) throws IOException {
    ImmutableMultimap.Builder<String, String> entryToContent = ImmutableMultimap.builder();

    try (ZipInputStream in =
        new ZipInputStream(new BufferedInputStream(Files.newInputStream(output.getPath())))) {
      for (ZipEntry e = in.getNextEntry(); e != null; e = in.getNextEntry()) {
        entryToContent.put(
            e.getName(), new String(ByteStreams.toByteArray(in), StandardCharsets.UTF_8).trim());
      }
    }

    return entryToContent.build();
  }
}
