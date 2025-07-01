/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.resources;

import static org.junit.Assert.assertEquals;

import com.facebook.buck.testutil.TemporaryPaths;
import com.facebook.buck.testutil.integration.TestDataHelper;
import com.google.common.collect.ImmutableList;
import com.google.common.io.ByteStreams;
import java.nio.ByteBuffer;
import java.nio.file.Path;
import java.util.zip.ZipFile;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

public class ChunkUtilsTest {
  private static final String APK_NAME = "example.apk";

  @Rule public TemporaryPaths tmpFolder = new TemporaryPaths();
  private Path apkPath;

  @Before
  public void setUp() {
    apkPath = TestDataHelper.getTestDataDirectory(this).resolve("aapt_dump").resolve(APK_NAME);
  }

  @Test
  public void testFindChunksInArsc() throws Exception {
    try (ZipFile apkZip = new ZipFile(apkPath.toFile())) {
      ByteBuffer buf =
          ResChunk.wrap(
              ByteStreams.toByteArray(apkZip.getInputStream(apkZip.getEntry("resources.arsc"))));

      assertEquals(
          ImmutableList.of(12, 660, 760), ChunkUtils.findChunks(buf, ResChunk.CHUNK_STRING_POOL));

      assertEquals(ImmutableList.of(0), ChunkUtils.findChunks(buf, ResChunk.CHUNK_RESOURCE_TABLE));
      assertEquals(
          ImmutableList.of(372), ChunkUtils.findChunks(buf, ResChunk.CHUNK_RES_TABLE_PACKAGE));
      assertEquals(
          ImmutableList.of(1024, 1040, 1320, 1436, 1624, 1896),
          ChunkUtils.findChunks(buf, ResChunk.CHUNK_RES_TABLE_TYPE_SPEC));
      assertEquals(
          ImmutableList.of(1072, 1196, 1340, 1468, 1652, 1920),
          ChunkUtils.findChunks(buf, ResChunk.CHUNK_RES_TABLE_TYPE));

      assertEquals(ImmutableList.of(), ChunkUtils.findChunks(buf, ResChunk.CHUNK_XML_TREE));
      assertEquals(ImmutableList.of(), ChunkUtils.findChunks(buf, ResChunk.CHUNK_XML_REF_MAP));
    }
  }

  @Test
  public void testFindChunksInManifest() throws Exception {
    try (ZipFile apkZip = new ZipFile(apkPath.toFile())) {
      ByteBuffer buf =
          ResChunk.wrap(
              ByteStreams.toByteArray(
                  apkZip.getInputStream(apkZip.getEntry("AndroidManifest.xml"))));

      assertEquals(ImmutableList.of(8), ChunkUtils.findChunks(buf, ResChunk.CHUNK_STRING_POOL));

      assertEquals(ImmutableList.of(), ChunkUtils.findChunks(buf, ResChunk.CHUNK_RESOURCE_TABLE));
      assertEquals(
          ImmutableList.of(), ChunkUtils.findChunks(buf, ResChunk.CHUNK_RES_TABLE_PACKAGE));
      assertEquals(ImmutableList.of(), ChunkUtils.findChunks(buf, ResChunk.CHUNK_RES_TABLE_TYPE));
      assertEquals(
          ImmutableList.of(), ChunkUtils.findChunks(buf, ResChunk.CHUNK_RES_TABLE_TYPE_SPEC));

      assertEquals(ImmutableList.of(0), ChunkUtils.findChunks(buf, ResChunk.CHUNK_XML_TREE));
      assertEquals(ImmutableList.of(1072), ChunkUtils.findChunks(buf, ResChunk.CHUNK_XML_REF_MAP));
    }
  }
}
