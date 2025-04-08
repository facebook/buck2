/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.io.file.FileExtensionMatcher;
import com.facebook.buck.io.file.PathMatcher;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlinSourceChanges;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

public class KotlinSourceChangesFactory {

  private static final Logger LOG = Logger.get(KotlinSourceChangesFactory.class);
  private static final PathMatcher KT_PATH_MATCHER = FileExtensionMatcher.of("kt");
  private static final PathMatcher JAVA_PATH_MATCHER = FileExtensionMatcher.of("java");

  // TODO(T210694438): use create(ActionMetadata) instead
  @Deprecated
  public static KotlinSourceChanges create() {
    return KotlinSourceChanges.ToBeCalculated.INSTANCE;
  }

  // TODO(ijurcikova): T210694438
  public static KotlinSourceChanges create(
      final AbsPath rootProjectDir, final ActionMetadata actionMetadata) {
    Map<Path, String> previousSourceFiles = filterSourceFiles(actionMetadata.getPreviousDigest());
    Map<Path, String> currentSourceFiles = filterSourceFiles(actionMetadata.getCurrentDigest());

    List<Path> modifiedFiles = getModifiedFiles(previousSourceFiles, currentSourceFiles);
    List<Path> removedFiles = getRemovedFiles(previousSourceFiles, currentSourceFiles);

    LOG.debug(
        "Source file changes: \n" + "modified files: [%s], \n" + "removed files: [%s]",
        modifiedFiles, removedFiles);

    return new KotlinSourceChanges.Known(
        modifiedFiles.stream().map(rootProjectDir::resolve).collect(Collectors.toList()),
        removedFiles.stream().map(rootProjectDir::resolve).collect(Collectors.toList()));
  }

  private static List<Path> getModifiedFiles(
      Map<Path, String> previousKotlinSourceFiles, Map<Path, String> currentKotlinSourceFiles) {
    List<Path> modifiedFiles = new ArrayList<>();

    for (Map.Entry<Path, String> entry : currentKotlinSourceFiles.entrySet()) {
      Path filePath = entry.getKey();
      String digest = entry.getValue();

      if (!Objects.equals(previousKotlinSourceFiles.get(filePath), digest)) {
        modifiedFiles.add(filePath);
      }
    }

    return modifiedFiles;
  }

  private static List<Path> getRemovedFiles(
      Map<Path, String> previousKotlinSourceFiles, Map<Path, String> currentKotlinSourceFiles) {
    List<Path> removedFiles = new ArrayList<>();

    for (Map.Entry<Path, String> entry : previousKotlinSourceFiles.entrySet()) {
      Path filePath = entry.getKey();

      if (!currentKotlinSourceFiles.containsKey(filePath)) {
        removedFiles.add(filePath);
      }
    }

    return removedFiles;
  }

  private static Map<Path, String> filterSourceFiles(Map<Path, String> digest) {
    return digest.entrySet().stream()
        .filter(
            pathStringEntry ->
                KT_PATH_MATCHER.matches(RelPath.of(pathStringEntry.getKey()))
                    || JAVA_PATH_MATCHER.matches(RelPath.of(pathStringEntry.getKey())))
        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
  }
}
