/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.abi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.testutil.TemporaryPaths;
import com.facebook.buck.testutil.integration.TestDataHelper;
import com.google.common.hash.Hashing;
import com.google.common.io.ByteSource;
import com.google.common.io.ByteStreams;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.Manifest;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

public class StubJarIntegrationTest {

  @Rule public TemporaryPaths temp = new TemporaryPaths();
  private Path testDataDir;
  private AbsPath root;

  @Before
  public void createWorkspace() {
    testDataDir = TestDataHelper.getTestDataDirectory(this).resolve("sample").toAbsolutePath();
    root = temp.getRoot();
  }

  @Test
  public void shouldBuildAbiJar() throws IOException {
    AbsPath out = root.resolve("junit-abi.jar");
    AbsPath regularJar = AbsPath.of(testDataDir.resolve("junit.jar"));
    new StubJar(regularJar, false).writeTo(out);

    // We assume that the lack of an exception indicates that the abi jar is correct. See MirrorTest
    // for why this is so.
    Path outputPath = out.getPath();
    assertTrue(Files.size(outputPath) > 0);
    assertTrue(Files.size(outputPath) < Files.size(regularJar.getPath()));
  }

  @Test
  public void shouldBuildAbiJarFromAbiJarWeCreated() throws IOException {
    AbsPath mid = root.resolve("junit-mid.jar");

    AbsPath source = AbsPath.of(testDataDir.resolve("junit.jar"));
    new StubJar(source, false).writeTo(mid);

    AbsPath out = root.resolve("junit-abi.jar");
    new StubJar(mid, false).writeTo(out);

    Path outputPath = out.getPath();
    assertTrue(Files.size(outputPath) > 0);
    assertEquals(Files.size(mid.getPath()), Files.size(outputPath));
  }

  @Test
  public void shouldBuildAbiJarFromAThirdPartyStubbedJar() throws IOException {
    AbsPath out = root.resolve("android-abi.jar");
    AbsPath source = AbsPath.of(testDataDir.resolve("android.jar"));
    new StubJar(source, false).writeTo(out);

    Path outputPath = out.getPath();
    assertTrue(Files.size(outputPath) > 0);
    assertTrue(Files.size(outputPath) < Files.size(source.getPath()));
  }

  @Test
  public void shouldBuildAbiJarEvenIfAsmWouldChokeOnAFrame() throws IOException {
    AbsPath out = root.resolve("unity-abi.jar");

    AbsPath source = AbsPath.of(testDataDir.resolve("unity.jar"));
    new StubJar(source, false).writeTo(out);

    Path outputPath = out.getPath();
    assertTrue(Files.size(outputPath) > 0);
    assertTrue(Files.size(outputPath) < Files.size(source.getPath()));
  }

  @Test
  public void abiJarManifestShouldContainHashesOfItsFiles() throws IOException {
    AbsPath out = root.resolve("junit-abi.jar");
    AbsPath regularJar = AbsPath.of(testDataDir.resolve("junit.jar"));
    new StubJar(regularJar, false).writeTo(out);

    try (JarFile stubJar = new JarFile(out.toFile())) {
      Manifest manifest = stubJar.getManifest();

      Enumeration<JarEntry> entries = stubJar.entries();
      while (entries.hasMoreElements()) {
        JarEntry entry = entries.nextElement();
        if (JarFile.MANIFEST_NAME.equals(entry.getName())) {
          continue;
        } else if (entry.getName().endsWith("/")) {
          assertNull(manifest.getAttributes(entry.getName()));
          continue;
        }

        String seenDigest = manifest.getAttributes(entry.getName()).getValue("Murmur3-128-Digest");

        String expectedDigest;
        try (InputStream inputStream = stubJar.getInputStream(entry)) {
          ByteSource byteSource = ByteSource.wrap(ByteStreams.toByteArray(inputStream));
          expectedDigest = byteSource.hash(Hashing.murmur3_128()).toString();
        }

        assertEquals(
            String.format("Digest mismatch for %s", entry.getName()), expectedDigest, seenDigest);
      }
    }
  }
}
