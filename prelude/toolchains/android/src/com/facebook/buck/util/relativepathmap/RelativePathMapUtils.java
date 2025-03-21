/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.relativepathmap;

import com.facebook.buck.core.exceptions.HumanReadableException;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

/** Utilities method related to process relative path maps */
public class RelativePathMapUtils {

  /** Adds path to relative map */
  public static void addPathToRelativePathMap(
      AbsPath rootPath,
      Map<Path, AbsPath> relativePathMap,
      Path basePath,
      AbsPath absolutePath,
      Path relativePath)
      throws IOException {
    if (Files.isDirectory(absolutePath.getPath())) {
      ImmutableSet<Path> files =
          ProjectFilesystemUtils.getFilesUnderPath(
              rootPath,
              absolutePath.getPath(),
              ProjectFilesystemUtils.getDefaultVisitOptions(),
              ProjectFilesystemUtils.getEmptyIgnoreFilter());
      for (Path file : files) {
        AbsPath absoluteFilePath = rootPath.resolve(file).normalize();
        addToRelativePathMap(
            relativePathMap, basePath.relativize(absoluteFilePath.getPath()), absoluteFilePath);
      }
    } else {
      addToRelativePathMap(relativePathMap, relativePath, absolutePath);
    }
  }

  private static void addToRelativePathMap(
      Map<Path, AbsPath> relativePathMap, Path pathRelativeToBaseDir, AbsPath absoluteFilePath) {
    relativePathMap.compute(
        pathRelativeToBaseDir,
        (ignored, current) -> {
          if (current != null) {
            throw new HumanReadableException(
                "The file '%s' appears twice in the hierarchy",
                pathRelativeToBaseDir.getFileName());
          }
          return absoluteFilePath;
        });
  }
}
