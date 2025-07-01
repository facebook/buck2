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

import com.facebook.buck.core.exceptions.HumanReadableException;
import com.facebook.buck.testutil.integration.TestDataHelper;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Pattern;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class ZipEntrySourceCollectionBuilderTest {

  @Rule public ExpectedException thrown = ExpectedException.none();

  private Path testDataDir;

  @Before
  public void setUp() {
    testDataDir = TestDataHelper.getTestDataScenario(this, "zip-collector");
  }

  @Test
  public void duplicateEntriesCauseFailure() {
    ZipEntrySourceCollectionBuilder builder =
        new ZipEntrySourceCollectionBuilder(ImmutableSet.of(), OnDuplicateEntry.FAIL);

    thrown.expect(HumanReadableException.class);
    thrown.expectMessage("Duplicate entry \"entry1\" is coming from entry11 and entry12");

    builder.addFile("entry1", Paths.get("entry11"));
    builder.addFile("entry1", Paths.get("entry12"));
  }

  @Test
  public void excludedDuplicateEntriesDoNotCauseFailure() {
    ZipEntrySourceCollectionBuilder builder =
        new ZipEntrySourceCollectionBuilder(
            ImmutableSet.of(Pattern.compile("entry1")), OnDuplicateEntry.FAIL);

    builder.addFile("entry1", Paths.get("entry11"));
    builder.addFile("entry1", Paths.get("entry12"));
  }

  @Test
  public void duplicateEntriesAreKeptInArchive() throws IOException {
    ZipEntrySourceCollectionBuilder builder =
        new ZipEntrySourceCollectionBuilder(ImmutableSet.of(), OnDuplicateEntry.APPEND);

    builder.addFile("tree.txt", testDataDir.resolve("tree.txt"));
    builder.addZipFile(testDataDir.resolve("zip-1.zip"));
    builder.addZipFile(testDataDir.resolve("zip-2.zip"));

    ZipEntrySourceCollection collection = builder.build();

    assertEquals(3, collection.getSources().size());
    assertEquals(
        ImmutableList.of("tree.txt", "tree.txt", "tree.txt"),
        collection.getSources().stream()
            .map(ZipEntrySource::getEntryName)
            .collect(ImmutableList.toImmutableList()));
    assertEquals(
        ImmutableList.of("tree.txt", "zip-1.zip", "zip-2.zip"),
        collection.getSources().stream()
            .map(ZipEntrySource::getSourceFilePath)
            .map(p -> testDataDir.relativize(p).toString())
            .collect(ImmutableList.toImmutableList()));
  }

  @Test
  public void duplicateEntriesAreNotKeptInArchiveWhenExcluded() throws IOException {
    ZipEntrySourceCollectionBuilder builder =
        new ZipEntrySourceCollectionBuilder(
            ImmutableSet.of(Pattern.compile("tree.txt")), OnDuplicateEntry.APPEND);

    builder.addFile("tree1.txt", testDataDir.resolve("tree.txt"));
    builder.addZipFile(testDataDir.resolve("zip-1.zip"));
    builder.addZipFile(testDataDir.resolve("zip-2.zip"));

    ZipEntrySourceCollection collection = builder.build();

    assertEquals(1, collection.getSources().size());
    assertEquals(
        ImmutableList.of("tree1.txt"),
        collection.getSources().stream()
            .map(ZipEntrySource::getEntryName)
            .collect(ImmutableList.toImmutableList()));
    assertEquals(
        ImmutableList.of("tree.txt"),
        collection.getSources().stream()
            .map(ZipEntrySource::getSourceFilePath)
            .map(p -> testDataDir.relativize(p).toString())
            .collect(ImmutableList.toImmutableList()));
  }

  @Test
  public void duplicateEntriesAreOverwrittenInArchive() throws IOException {
    ZipEntrySourceCollectionBuilder builder =
        new ZipEntrySourceCollectionBuilder(ImmutableSet.of(), OnDuplicateEntry.OVERWRITE);

    builder.addFile("tree.txt", testDataDir.resolve("tree.txt"));
    builder.addZipFile(testDataDir.resolve("zip-1.zip"));
    builder.addZipFile(testDataDir.resolve("zip-2.zip"));

    ZipEntrySourceCollection collection = builder.build();

    assertEquals(1, collection.getSources().size());
    assertEquals(
        ImmutableList.of("tree.txt"),
        collection.getSources().stream()
            .map(ZipEntrySource::getEntryName)
            .collect(ImmutableList.toImmutableList()));
    assertEquals(
        ImmutableList.of("zip-2.zip"),
        collection.getSources().stream()
            .map(ZipEntrySource::getSourceFilePath)
            .map(p -> testDataDir.relativize(p).toString())
            .collect(ImmutableList.toImmutableList()));
  }
}
