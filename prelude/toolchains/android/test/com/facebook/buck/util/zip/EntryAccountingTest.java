/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.function.Consumer;
import java.util.zip.CRC32;
import java.util.zip.ZipEntry;
import org.junit.Test;

public class EntryAccountingTest {

  private static final byte[] CONTENT =
      ("This is test content to compress " + System.currentTimeMillis())
          .getBytes(StandardCharsets.UTF_8);

  @Test
  public void testCompressionMethod() {
    assertEquals(
        ZipEntry.STORED, newEntry(e -> e.setMethod(ZipEntry.STORED)).getCompressionMethod());
    assertEquals(
        ZipEntry.DEFLATED, newEntry(e -> e.setMethod(ZipEntry.DEFLATED)).getCompressionMethod());
  }

  @Test
  public void testSetTime() {

    final EntryAccounting unset = newEntry(e -> e.setTime(-1));
    final EntryAccounting old =
        newEntry(
            e ->
                e.setTime(
                    LocalDate.of(1979, 1, 1)
                        .atStartOfDay()
                        .toInstant(ZoneOffset.UTC)
                        .toEpochMilli()));

    assertTrue(old.getTime() == ZipConstants.DOS_FAKE_TIME);
    assertTrue(unset.getTime() > ZipConstants.DOS_FAKE_TIME);
  }

  @Test
  public void testStoredMethod() throws IOException {

    final CRC32 crc32 = new CRC32();
    final ByteArrayOutputStream output = new ByteArrayOutputStream();

    final EntryAccounting entry =
        newEntry(
            e -> {
              e.setMethod(ZipEntry.STORED);
              e.setSize(CONTENT.length);
              e.setCompressedSize(CONTENT.length);
              // generate crc32 to compare
              crc32.update(CONTENT);
              e.setCrc(crc32.getValue());
            });

    entry.write(output, CONTENT);
    final long bytesWritten = entry.finish(output);

    assertEquals(CONTENT.length, bytesWritten);
    assertEquals(CONTENT.length, output.size());
    assertEquals(crc32.getValue(), entry.getCrc());
    assertArrayEquals(CONTENT, output.toByteArray());
  }

  @Test
  public void testSizeRequiredForStoredMethod() throws IOException {
    try {
      final ByteArrayOutputStream out = new ByteArrayOutputStream();
      final EntryAccounting entryAccounting =
          newEntry(
              e -> {
                e.setMethod(ZipEntry.STORED);
                e.setSize(CONTENT.length);
              });
      entryAccounting.write(out, CONTENT);
      entryAccounting.finish(out);
      fail();
    } catch (IllegalStateException e) {
      assertTrue(e.getMessage().contains("Number of bytes"));
    }
  }

  @Test
  public void testCrcRequiredForStoredMethod() throws IOException {
    try {
      final ByteArrayOutputStream out = new ByteArrayOutputStream();
      final EntryAccounting entryAccounting =
          newEntry(
              e -> {
                e.setMethod(ZipEntry.STORED);
                e.setSize(CONTENT.length);
                e.setCompressedSize(CONTENT.length);
              });
      entryAccounting.write(out, CONTENT);
      entryAccounting.finish(out);
      fail();
    } catch (IllegalStateException e) {
      assertTrue(e.getMessage().contains("CRC of bytes"));
    }
  }

  @Test
  public void testDeflatedMethod() throws IOException {

    final CRC32 crc32 = new CRC32();
    final ByteArrayOutputStream output = new ByteArrayOutputStream();
    final EntryAccounting accounting = newEntry(e -> {});

    crc32.update(CONTENT);
    accounting.write(output, CONTENT);
    long bytesWritten = accounting.finish(output);

    // For deflated entries, the size should be set during finish
    assertEquals(CONTENT.length, accounting.getEntry().getSize());
    assertTrue(accounting.getEntry().getCompressedSize() > 0);
    assertEquals(crc32.getValue(), accounting.getEntry().getCrc());
    // The total bytes written should include the data descriptor
    assertTrue(bytesWritten > 0);
    assertTrue(bytesWritten >= accounting.getEntry().getCompressedSize());
  }

  private EntryAccounting newEntry(Consumer<CustomZipEntry> setter) {
    final CustomZipEntry entry = new CustomZipEntry("test");
    setter.accept(entry);
    return new EntryAccounting(entry, 0);
  }
}
