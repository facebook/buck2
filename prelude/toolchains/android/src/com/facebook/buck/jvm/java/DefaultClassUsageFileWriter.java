/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.core.exceptions.HumanReadableException;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.facebook.buck.util.json.ObjectMappers;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedMap;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;

public class DefaultClassUsageFileWriter implements ClassUsageFileWriter {
  public static final String ROOT_CELL_IDENTIFIER = "_";

  @Override
  public void writeFile(
      ImmutableMap<Path, Map<Path, Integer>> classUsageMap,
      RelPath relativePath,
      AbsPath rootPath,
      RelPath configuredBuckOut) {
    try {
      AbsPath parent = rootPath.resolve(relativePath).getParent();
      Path parentRelativePath = rootPath.relativize(parent).getPath();
      Preconditions.checkState(
          ProjectFilesystemUtils.exists(rootPath, parentRelativePath),
          "directory must exist: %s",
          parent);

      ObjectMappers.WRITER.writeValue(
          rootPath.resolve(relativePath).toFile(),
          relativizeMap(classUsageMap, rootPath, configuredBuckOut));
    } catch (IOException e) {
      throw new HumanReadableException(e, "Unable to write used classes file.");
    }
  }

  protected static ImmutableSortedMap<Path, Map<Path, Integer>> relativizeMap(
      ImmutableMap<Path, Map<Path, Integer>> classUsageMap,
      AbsPath rootPath,
      RelPath configuredBuckOut) {
    ImmutableSortedMap.Builder<Path, Map<Path, Integer>> builder =
        ImmutableSortedMap.naturalOrder();

    for (Map.Entry<Path, Map<Path, Integer>> jarClassesEntry : classUsageMap.entrySet()) {
      Path jarAbsolutePath = jarClassesEntry.getKey();
      // Don't include jars that are outside of the project
      // Paths outside the project would make these class usage files problematic for caching.
      // Fortunately, such paths are also not interesting for the main use cases that these files
      // address, namely understanding the dependencies one java rule has on others. Jar files
      // outside the project are coming from a build tool (e.g. JDK or Android SDK).
      // Here we first check to see if the path is inside the root filesystem of the build
      // If not, we check to see if it's under one of the other cell roots.
      // If the the absolute path does not reside under any cell root, we exclude it
      ProjectFilesystemUtils.getPathRelativeToProjectRoot(
              rootPath, configuredBuckOut, jarAbsolutePath)
          .ifPresent(
              projectPath ->
                  builder.put(projectPath, ImmutableSortedMap.copyOf(jarClassesEntry.getValue())));
    }

    return builder.build();
  }
}
