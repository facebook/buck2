/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin;

import static com.facebook.buck.jvm.java.CompilerOutputPaths.getKAPTDepFilePath;
import static com.facebook.buck.jvm.java.CompilerOutputPaths.getKotlinTempDepFilePath;
import static com.facebook.buck.jvm.java.CompilerOutputPaths.getKspDepFilePath;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.java.ClassUsageURIParser;
import com.facebook.buck.jvm.kotlin.compilerplugins.usedclasses.ClassUsageMerger;
import com.facebook.buck.util.json.ObjectMappers;
import com.fasterxml.jackson.core.type.TypeReference;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ImmutableMap;
import java.io.BufferedReader;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Java used-class optimization directly inject FileManager into javac process. However, that is no
 * longer possible for Kotlin. We have to modify KAPT, KSP and compiler plugin to output
 * dependencies into separate report files, then parse and merge them to get a complete map of class
 * usages.
 *
 * <p>This class includes utility functions for parsing and merging kotlin temp dep files.
 */
public class KotlinClassUsageHelper {

  /**
   * Returns a map of all the files used by the Kotlin compiler and the its annotation processors.
   */
  static ImmutableMap<Path, Set<Path>> getClassUsageData(
      RelPath reportDirPath, AbsPath ruleCellRoot) throws IOException {
    // read compiler plugin generated class usage report file
    Path kotlinGeneralClassUsageReportPath =
        ruleCellRoot.resolve(getKotlinTempDepFilePath(reportDirPath)).getPath();
    ImmutableMap<Path, Set<Path>> classUsages =
        readNdJsonBasedClassUsageReport(kotlinGeneralClassUsageReportPath);

    // merge kapt generated class usage report file if it exist
    Path kaptClassUsageReportPath =
        ruleCellRoot.resolve(getKAPTDepFilePath(reportDirPath)).getPath();
    if (kaptClassUsageReportPath.toFile().exists()) {
      classUsages = merge(classUsages, readUriBasedClassUsageFile(kaptClassUsageReportPath));
    }

    // merge ksp generated class usage report file if it exist
    Path kspClassUsageReportPath = ruleCellRoot.resolve(getKspDepFilePath(reportDirPath)).getPath();
    if (kspClassUsageReportPath.toFile().exists()) {
      classUsages = merge(classUsages, readUriBasedClassUsageFile(kspClassUsageReportPath));
    }

    return classUsages;
  }

  /**
   * Read a class usage report that is in desired json format already
   *
   * <p>Kotlin dep-tracker compiler plugin, for example, is internal, so we were managed to have it
   * output in the desired format since beginning.
   */
  @VisibleForTesting
  static ImmutableMap<Path, Map<Path, Integer>> readJsonBasedClassUsageReport(Path path)
      throws IOException {
    if (!Files.exists(path)) {
      return ImmutableMap.of();
    }

    return ObjectMappers.readValue(path, new TypeReference<>() {});
  }

  /** Read a class usage report that is in desired ndJson format already */
  @VisibleForTesting
  static ImmutableMap<Path, Set<Path>> readNdJsonBasedClassUsageReport(Path path)
      throws IOException {
    if (!Files.exists(path)) {
      return ImmutableMap.of();
    }

    Map<Path, Set<Path>> resultMap = new HashMap<>();
    try (BufferedReader reader = Files.newBufferedReader(path)) {
      String line;
      while ((line = reader.readLine()) != null) {
        Map<Path, Set<Path>> classUsageMap =
            ObjectMappers.readValue(line, new TypeReference<>() {});

        resultMap = ClassUsageMerger.mergeClassUsageMaps(resultMap, classUsageMap);
      }
    }

    return ImmutableMap.copyOf(resultMap);
  }

  /**
   * Read a class usage report that is in the raw format one URI per line
   *
   * <p>KAPT output in this format for example.
   */
  @VisibleForTesting
  static ImmutableMap<Path, Set<Path>> readUriBasedClassUsageFile(Path kaptClassUsageFilePath)
      throws IOException {
    final ClassUsageURIParser parser = new ClassUsageURIParser();
    Files.readAllLines(kaptClassUsageFilePath)
        .forEach(line -> parser.parseAndRecordURI(URI.create(line)));
    return parser.getClassUsageMap();
  }

  /** Merge two class usage map */
  @VisibleForTesting
  static ImmutableMap<Path, Set<Path>> merge(Map<Path, Set<Path>> mapA, Map<Path, Set<Path>> mapB) {
    // Merge map A to resultBuilder, and ensure both inner and outer map is mutable
    HashMap<Path, Set<Path>> resultBuilder = new HashMap<>();
    for (Map.Entry<Path, Set<Path>> jarEntry : mapA.entrySet()) {
      resultBuilder.put(jarEntry.getKey(), new HashSet<>(jarEntry.getValue()));
    }
    // Merge map B to result builder. Increment count if same entry exist.
    for (Map.Entry<Path, Set<Path>> jarEntry : mapB.entrySet()) {
      Set<Path> resultClassPaths =
          resultBuilder.computeIfAbsent(jarEntry.getKey(), _path -> new HashSet<>());
      resultClassPaths.addAll(jarEntry.getValue());
    }
    return ImmutableMap.copyOf(resultBuilder);
  }
}
