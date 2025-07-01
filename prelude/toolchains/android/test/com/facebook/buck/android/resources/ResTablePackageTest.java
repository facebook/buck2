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
import com.google.common.collect.ImmutableList;
import com.google.common.io.ByteStreams;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipFile;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

public class ResTablePackageTest {
  private static final String APK_NAME = "example.apk";

  @Rule public TemporaryPaths tmpFolder = new TemporaryPaths();
  private Path apkPath;

  @Before
  public void setUp() {
    apkPath = TestDataHelper.getTestDataDirectory(this).resolve("aapt_dump").resolve(APK_NAME);
  }

  @Test
  public void testGetAndSerialize() throws Exception {
    try (ZipFile apkZip = new ZipFile(apkPath.toFile())) {
      ByteBuffer buf =
          ResChunk.wrap(
              ByteStreams.toByteArray(apkZip.getInputStream(apkZip.getEntry("resources.arsc"))));

      List<Integer> offsets = ChunkUtils.findChunks(buf, ResChunk.CHUNK_RES_TABLE_PACKAGE);
      assertEquals(ImmutableList.of(372), offsets);

      int offset = 372;
      ByteBuffer data = ResChunk.slice(buf, offset);
      ResTablePackage resPackage = ResTablePackage.get(data);

      byte[] expected =
          Arrays.copyOfRange(
              data.array(), data.arrayOffset(), data.arrayOffset() + resPackage.getTotalSize());
      byte[] actual = resPackage.serialize();

      assertArrayEquals(expected, actual);
    }
  }

  @Test
  public void testFullSliceResTablePackage() throws Exception {
    try (ZipFile apkZip = new ZipFile(apkPath.toFile())) {
      ByteBuffer buf =
          ResChunk.wrap(
              ByteStreams.toByteArray(apkZip.getInputStream(apkZip.getEntry("resources.arsc"))));

      ResourceTable resourceTable = ResourceTable.get(buf);
      ResTablePackage resPackage = resourceTable.getPackage();
      Map<Integer, Integer> counts = new HashMap<>();
      for (ResTableTypeSpec spec : resPackage.getTypeSpecs()) {
        counts.put(spec.getResourceType(), spec.getEntryCount());
      }
      ResTablePackage copy = ResTablePackage.slice(resPackage, counts);

      ByteArrayOutputStream baos = new ByteArrayOutputStream();
      resPackage.dump(resourceTable.getStrings(), new PrintStream(baos));
      String expected = new String(baos.toByteArray(), StandardCharsets.UTF_8);

      baos = new ByteArrayOutputStream();
      copy.dump(resourceTable.getStrings(), new PrintStream(baos));
      String content = new String(baos.toByteArray(), StandardCharsets.UTF_8);

      MoreAsserts.assertLargeStringsEqual(expected, content);
    }
  }
}
