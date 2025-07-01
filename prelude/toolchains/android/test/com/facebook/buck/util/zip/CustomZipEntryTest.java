/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util.zip;

import static java.util.zip.Deflater.BEST_COMPRESSION;
import static java.util.zip.Deflater.DEFAULT_COMPRESSION;
import static java.util.zip.Deflater.NO_COMPRESSION;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;

import com.google.common.hash.HashCode;
import com.google.common.hash.Hashing;
import java.io.FileOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.TimeZone;
import java.util.jar.JarOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import org.hamcrest.Matchers;
import org.junit.Test;

public class CustomZipEntryTest {

  @Test
  public void shouldChangeMethodWhenCompressionLevelIsChanged() {
    CustomZipEntry entry = new CustomZipEntry("cake");
    assertEquals(ZipEntry.DEFLATED, entry.getMethod());
    assertEquals(DEFAULT_COMPRESSION, entry.getCompressionLevel());

    entry.setCompressionLevel(NO_COMPRESSION);
    assertEquals(ZipEntry.STORED, entry.getMethod());
    assertEquals(NO_COMPRESSION, entry.getCompressionLevel());

    entry.setCompressionLevel(BEST_COMPRESSION);
    assertEquals(ZipEntry.DEFLATED, entry.getMethod());
    assertEquals(BEST_COMPRESSION, entry.getCompressionLevel());
  }

  @Test
  public void producedZipFilesAreTimezoneAgnostic() throws Exception {
    HashCode referenceHash = writeSimpleJarAndGetHash();
    TimeZone previousDefault = TimeZone.getDefault();
    try {
      String[] availableIDs = TimeZone.getAvailableIDs();
      assertThat(availableIDs.length, Matchers.greaterThan(1));

      for (String timezoneID : availableIDs) {
        TimeZone timeZone = TimeZone.getTimeZone(timezoneID);
        TimeZone.setDefault(timeZone);

        assertThat(writeSimpleJarAndGetHash(), Matchers.equalTo(referenceHash));
      }
    } finally {
      TimeZone.setDefault(previousDefault);
    }
  }

  private HashCode writeSimpleJarAndGetHash() throws Exception {
    Path output = Files.createTempFile("example", ".jar");
    try (FileOutputStream fileOutputStream = new FileOutputStream(output.toFile());
        ZipOutputStream out = new JarOutputStream(fileOutputStream)) {
      ZipEntry entry = new CustomZipEntry("test");
      out.putNextEntry(entry);
      out.write(new byte[0]);
      entry = new ZipEntry("test1");
      entry.setTime(ZipConstants.getFakeTime());
      out.putNextEntry(entry);
      out.write(new byte[0]);
    }

    return Hashing.sha1().hashBytes(Files.readAllBytes(output));
  }
}
