/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import com.facebook.buck.cd.model.java.AbiGenerationMode;
import com.facebook.buck.core.build.execution.context.IsolatedExecutionContext;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.step.TestExecutionContext;
import com.facebook.buck.testutil.TemporaryPaths;
import com.facebook.buck.testutil.integration.TestDataHelper;
import com.facebook.buck.testutil.integration.ZipInspector;
import com.facebook.buck.util.environment.Platform;
import com.facebook.buck.util.zip.CustomJarOutputStream;
import com.google.common.hash.Hashing;
import com.google.common.io.MoreFiles;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.jar.JarFile;
import java.util.jar.Manifest;
import org.junit.Rule;
import org.junit.Test;

public class CalculateClassAbiStepTest {
  @Rule public TemporaryPaths temp = new TemporaryPaths();

  @Test
  public void shouldCalculateAbiFromAStubJar() throws IOException {
    AbsPath outDir = temp.newFolder();

    Path directory = TestDataHelper.getTestDataDirectory(this);
    Path source = directory.resolve("prebuilt/junit.jar");
    RelPath binJar = RelPath.get("source.jar");
    Files.copy(source, outDir.resolve(binJar).getPath());
    RelPath abiJar = RelPath.get("abi.jar");
    RelPath jvmAbiGenDir = RelPath.get("abi_tmp");

    IsolatedExecutionContext executionContext = TestExecutionContext.newInstance(outDir);
    new CalculateClassAbiStep(binJar, jvmAbiGenDir, abiJar, AbiGenerationMode.CLASS)
        .executeIsolatedStep(executionContext);

    Path abiJarPath = outDir.resolve(abiJar).getPath();
    String seenHash = MoreFiles.asByteSource(abiJarPath).hash(Hashing.sha1()).toString();

    // Hi there! This is hardcoded here because we want to make sure buck always produces the same
    // jar files across timezones and versions. If the test is failing because of an intentional
    // modification to how we produce abi .jar files, then just update the hash, otherwise please
    // investigate why the value is different.
    // NOTE: If this starts failing on CI for no obvious reason it's possible that the offset
    // calculation in ZipConstants.getFakeTime() does not account for DST correctly.
    // Note Mac and Linux produce jar files with different hashes, but the contained files are
    // identical.
    String expectedHash =
        switch (Platform.detect()) {
          case MACOS -> "51b28115808a8684550a7b026154a94075358b68";
          case LINUX -> "385fb1f20dcde21864a51a592eac0a0f3eaf7ba6";
          default -> throw new IllegalStateException("Unsupported platform");
        };
    assertEquals(expectedHash, seenHash);

    // Assert that the abiJar contains non-class resources (like txt files).
    ZipInspector inspector = new ZipInspector(abiJarPath);
    inspector.assertFileExists("LICENSE.txt");

    try (JarFile jarFile = new JarFile(abiJarPath.toFile())) {
      Manifest manifest = jarFile.getManifest();
      assertNotNull(
          manifest
              .getAttributes("junit/runner/BaseTestRunner.class")
              .getValue(CustomJarOutputStream.DIGEST_ATTRIBUTE_NAME));
    }
  }
}
