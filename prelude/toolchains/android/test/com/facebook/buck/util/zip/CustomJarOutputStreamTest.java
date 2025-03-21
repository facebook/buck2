/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.zip;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import com.google.common.hash.Hashing;
import com.google.common.hash.HashingInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarInputStream;
import java.util.jar.Manifest;
import org.junit.Before;
import org.junit.Test;

public class CustomJarOutputStreamTest {
  private ByteArrayOutputStream out;
  private CustomJarOutputStream writer;

  @Before
  public void setUp() {
    out = new ByteArrayOutputStream();
    writer = ZipOutputStreams.newJarOutputStream(out);
    writer.setEntryHashingEnabled(true);
  }

  @Test
  public void entriesAreWrittenAsTheyAreEncounteredWithManifestLast() throws IOException {
    writer.writeEntry(
        "Z", new ByteArrayInputStream("Z's contents".getBytes(StandardCharsets.UTF_8)));
    writer.writeEntry(
        "A", new ByteArrayInputStream("A's contents".getBytes(StandardCharsets.UTF_8)));
    writer.close();

    try (JarInputStream jar = new JarInputStream(new ByteArrayInputStream(out.toByteArray()))) {
      JarEntry entry;
      entry = jar.getNextJarEntry();
      assertEquals("Z", entry.getName());

      entry = jar.getNextJarEntry();
      assertEquals("A", entry.getName());

      entry = jar.getNextJarEntry();
      assertEquals(JarFile.MANIFEST_NAME, entry.getName());
    }
  }

  @Test
  public void manifestContainsEntryHashesOfHashedEntries() throws IOException {
    String entryName = "A";
    InputStream contents = new ByteArrayInputStream("contents".getBytes(StandardCharsets.UTF_8));
    try (HashingInputStream hashingContents =
        new HashingInputStream(Hashing.murmur3_128(), contents)) {
      writer.writeEntry(entryName, hashingContents);
      writer.close();

      String expectedHash = hashingContents.hash().toString();
      assertEntryHash(entryName, expectedHash);
    }
  }

  @Test
  public void manifestContainsEntryHashesOfEmptyHashedEntries() throws IOException {
    String entryName = "A";
    InputStream contents = new ByteArrayInputStream(new byte[0]);
    try (HashingInputStream hashingContents =
        new HashingInputStream(Hashing.murmur3_128(), contents)) {
      writer.putNextEntry(new CustomZipEntry(entryName));
      writer.closeEntry();
      writer.close();

      String expectedHash = hashingContents.hash().toString();
      assertEntryHash(entryName, expectedHash);
    }
  }

  @Test
  public void manifestDoesNotContainEntryHashesOfDirectories() throws IOException {
    String entryName = "A/";
    writer.putNextEntry(new CustomZipEntry(entryName));
    writer.closeEntry();
    writer.close();

    assertNoEntryHash(entryName);
  }

  private void assertEntryHash(String entryName, String expectedHash) throws IOException {
    Manifest manifest = getManifest();
    assertEquals(expectedHash, manifest.getEntries().get(entryName).getValue("Murmur3-128-Digest"));
  }

  private void assertNoEntryHash(String entryName) throws IOException {
    assertFalse(getManifest().getEntries().containsKey(entryName));
  }

  private Manifest getManifest() throws IOException {
    Manifest manifest = new Manifest();
    try (JarInputStream jar = new JarInputStream(new ByteArrayInputStream(out.toByteArray()))) {
      jar.getNextJarEntry();
      JarEntry manifestEntry = jar.getNextJarEntry();
      assertEquals(JarFile.MANIFEST_NAME, manifestEntry.getName());
      manifest.read(jar);
    }
    return manifest;
  }
}
