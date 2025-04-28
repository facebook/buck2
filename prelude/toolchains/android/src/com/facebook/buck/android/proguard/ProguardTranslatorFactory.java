/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.proguard;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Functions;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Iterables;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

/**
 * If we end up creating both an obfuscator function and a deobfuscator function, it would be nice
 * to load the proguard mapping file once. This class enables sharing that work.
 */
public class ProguardTranslatorFactory {

  private final Optional<ImmutableMap<String, String>> rawMap;

  private ProguardTranslatorFactory(Optional<ImmutableMap<String, String>> rawMap) {
    this.rawMap = rawMap;
  }

  public static ProguardTranslatorFactory create(
      Optional<Path> proguardFullConfigFile,
      Optional<Path> proguardMappingFile,
      boolean skipProguard)
      throws IOException {
    return new ProguardTranslatorFactory(
        loadOptionalRawMap(proguardFullConfigFile, proguardMappingFile, skipProguard));
  }

  @VisibleForTesting
  public static ProguardTranslatorFactory createForTest(
      Optional<ImmutableMap<String, String>> rawMap) {
    return new ProguardTranslatorFactory(rawMap);
  }

  private static Optional<ImmutableMap<String, String>> loadOptionalRawMap(
      Optional<Path> proguardFullConfigFile,
      Optional<Path> proguardMappingFile,
      boolean skipProguard)
      throws IOException {
    if (skipProguard || !proguardFullConfigFile.isPresent()) {
      return Optional.empty();
    }

    Path pathToProguardConfig = proguardFullConfigFile.get();

    // Proguard doesn't print a mapping when obfuscation is disabled.
    boolean obfuscationSkipped =
        Iterables.any(Files.readAllLines(pathToProguardConfig), "-dontobfuscate"::equals);
    if (obfuscationSkipped) {
      return Optional.empty();
    }

    Path mappingFile = proguardMappingFile.get();
    List<String> lines = Files.readAllLines(mappingFile);
    return Optional.of(ProguardMapping.readClassMapping(lines));
  }

  public Function<String, String> createDeobfuscationFunction() {
    return createFunction(false, false);
  }

  public Function<String, String> createObfuscationFunction() {
    return createFunction(true, false);
  }

  public Function<String, String> createNullableObfuscationFunction() {
    return createFunction(true, true);
  }

  private Function<String, String> createFunction(
      final boolean isForObfuscation, final boolean isNullable) {
    if (!rawMap.isPresent()) {
      return Functions.identity();
    }

    ImmutableMap.Builder<String, String> builder = ImmutableMap.builder();
    for (Map.Entry<String, String> entry : rawMap.get().entrySet()) {
      String original = entry.getKey().replace('.', '/');
      String obfuscated = entry.getValue().replace('.', '/');
      builder.put(
          isForObfuscation ? original : obfuscated, isForObfuscation ? obfuscated : original);
    }
    Map<String, String> map = builder.build();

    return input -> {
      String mapped = map.get(input);
      if (isNullable || mapped != null) {
        return mapped;
      } else {
        return input;
      }
    };
  }
}
