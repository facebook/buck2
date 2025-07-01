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

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import com.facebook.buck.testutil.MoreAsserts;
import com.facebook.buck.testutil.TemporaryPaths;
import com.facebook.buck.testutil.integration.TestDataHelper;
import com.google.common.io.ByteStreams;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.zip.ZipFile;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

public class StringPoolTest {
  private static final String APK_NAME = "example.apk";

  @Rule public TemporaryPaths tmpFolder = new TemporaryPaths();
  private Path apkPath;
  private Path testDataDir;

  @Before
  public void setUp() {
    testDataDir = TestDataHelper.getTestDataDirectory(this).resolve("aapt_dump");
    apkPath = testDataDir.resolve(APK_NAME);
  }

  @Test
  public void testCreateStringPool() {
    String[] strings = new String[] {"string1", "string2", "string3"};
    StringPool pool = StringPool.create(Arrays.asList(strings));
    assertEquals(strings.length, pool.getStringCount());
    for (int i = 0; i < strings.length; i++) {
      assertEquals(strings[i], pool.getString(i));
    }
  }

  @Test
  public void testCreateSerializeAndGet() {
    String[] strings = new String[] {"string1", "string2", "string3"};
    StringPool pool = StringPool.create(Arrays.asList(strings));
    byte[] serialized = pool.serialize();
    pool = StringPool.get(ResChunk.wrap(serialized));

    assertEquals(strings.length, pool.getStringCount());
    for (int i = 0; i < strings.length; i++) {
      assertEquals(strings[i], pool.getString(i));
    }

    byte[] reserialized = pool.serialize();
    assertArrayEquals(serialized, reserialized);
  }

  @Test
  public void testArscPoolGetAndSerialize() throws Exception {
    try (ZipFile apkZip = new ZipFile(apkPath.toFile())) {
      ByteBuffer buf =
          ResChunk.wrap(
              ByteStreams.toByteArray(apkZip.getInputStream(apkZip.getEntry("resources.arsc"))));

      List<Integer> stringPoolOffsets = ChunkUtils.findChunks(buf, ResChunk.CHUNK_STRING_POOL);
      // Should be 1 main stringpool, and 2 for the package (types and keys).
      assertEquals(3, stringPoolOffsets.size());

      for (int offset : stringPoolOffsets) {
        ByteBuffer data = ResChunk.slice(buf, offset);
        StringPool strings = StringPool.get(data);

        byte[] expected =
            Arrays.copyOfRange(
                data.array(), data.arrayOffset(), data.arrayOffset() + strings.getTotalSize());
        byte[] actual = strings.serialize();

        assertArrayEquals(expected, actual);
      }
    }
  }

  @Test
  public void testManifestPoolGetAndSerialize() throws Exception {
    try (ZipFile apkZip = new ZipFile(apkPath.toFile())) {
      ByteBuffer buf =
          ResChunk.wrap(
              ByteStreams.toByteArray(
                  apkZip.getInputStream(apkZip.getEntry("AndroidManifest.xml"))));

      List<Integer> stringPoolOffsets = ChunkUtils.findChunks(buf, ResChunk.CHUNK_STRING_POOL);
      assertEquals(1, stringPoolOffsets.size());

      for (int offset : stringPoolOffsets) {
        ByteBuffer data = ResChunk.slice(buf, offset);
        StringPool strings = StringPool.get(data);

        byte[] expected =
            Arrays.copyOfRange(
                data.array(), data.arrayOffset(), data.arrayOffset() + strings.getTotalSize());
        byte[] actual = strings.serialize();

        assertArrayEquals(expected, actual);
      }
    }
  }

  @Test
  public void testAaptDumpStrings() throws Exception {
    try (ZipFile apkZip = new ZipFile(apkPath.toFile())) {
      ByteBuffer buf =
          ResChunk.wrap(
              ByteStreams.toByteArray(apkZip.getInputStream(apkZip.getEntry("resources.arsc"))));
      ResourceTable resourceTable = ResourceTable.get(buf);
      ByteArrayOutputStream baos = new ByteArrayOutputStream();

      resourceTable.dumpStrings(new PrintStream(baos));

      String content = new String(baos.toByteArray(), StandardCharsets.UTF_8);

      Path stringsOutput = testDataDir.resolve(APK_NAME + ".strings");
      String expected = new String(Files.readAllBytes(stringsOutput));

      MoreAsserts.assertLargeStringsEqual(expected, content);
    }
  }
}
