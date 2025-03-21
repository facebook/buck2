/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.proguard;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import com.google.common.collect.ImmutableList;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class ProguardTranslatorFactoryTest {
  @Rule public TemporaryFolder tempDir = new TemporaryFolder();

  @Test
  public void testEnableObfuscation() throws IOException {
    Path proguardConfigFile = tempDir.newFile("configuration.txt").toPath();
    Path proguardMappingFile = tempDir.newFile("mapping.txt").toPath();

    List<String> linesInMappingFile =
        ImmutableList.of(
            "foo.bar.MappedPrimary -> foo.bar.a:",
            "foo.bar.UnmappedPrimary -> foo.bar.UnmappedPrimary:",
            "foo.primary.MappedPackage -> x.a:");

    Files.write(proguardConfigFile, ImmutableList.of());
    Files.write(proguardMappingFile, linesInMappingFile);

    ProguardTranslatorFactory translatorFactory =
        ProguardTranslatorFactory.create(
            Optional.of(proguardConfigFile), Optional.of(proguardMappingFile), false);
    checkMapping(translatorFactory, "foo/bar/MappedPrimary", "foo/bar/a");
    checkMapping(translatorFactory, "foo/bar/UnmappedPrimary", "foo/bar/UnmappedPrimary");
    checkMapping(translatorFactory, "foo/primary/MappedPackage", "x/a");

    assertNull(translatorFactory.createNullableObfuscationFunction().apply("foo/bar/NotInMapping"));
  }

  @Test
  public void testDisableObfuscation() throws IOException {
    Path proguardConfigFile = tempDir.newFile("configuration.txt").toPath();
    Path proguardMappingFile = tempDir.newFile("mapping.txt").toPath();
    Files.write(proguardConfigFile, ImmutableList.of("-dontobfuscate"));

    ProguardTranslatorFactory translatorFactory =
        ProguardTranslatorFactory.create(
            Optional.of(proguardConfigFile), Optional.of(proguardMappingFile), false);
    checkMapping(translatorFactory, "anything", "anything");
  }

  private void checkMapping(
      ProguardTranslatorFactory translatorFactory, String original, String obfuscated) {
    assertEquals(original, translatorFactory.createDeobfuscationFunction().apply(obfuscated));
    assertEquals(obfuscated, translatorFactory.createObfuscationFunction().apply(original));
    assertEquals(obfuscated, translatorFactory.createNullableObfuscationFunction().apply(original));
  }
}
