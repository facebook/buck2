/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.resources.filter;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.io.filesystem.CopySourceMode;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Map;
import java.util.function.Predicate;

/**
 * This class allows the creation of copies of multiple directories, while filtering out files which
 * do not match a specified predicate.
 *
 * <p>Current caveats:
 *
 * <ul>
 *   <li>Existing content in destination directories is deleted.
 *   <li>Empty directories will not be created.
 * </ul>
 */
public class FilteredDirectoryCopier {

  private FilteredDirectoryCopier() {}

  public static void copyDirs(
      AbsPath projectRoot,
      DirectoryStream.Filter<? super Path> ignoreFilter,
      Map<Path, Path> sourcesToDestinations,
      Predicate<Path> pred)
      throws IOException {
    for (Map.Entry<Path, Path> e : sourcesToDestinations.entrySet()) {
      copyDir(projectRoot, ignoreFilter, e.getKey(), e.getValue(), pred);
    }
  }

  private static void copyDir(
      AbsPath projectRoot,
      DirectoryStream.Filter<? super Path> ignoreFilter,
      Path srcDir,
      Path destDir,
      Predicate<Path> pred)
      throws IOException {

    // Remove existing contents if any.
    if (ProjectFilesystemUtils.exists(projectRoot, destDir)) {
      ProjectFilesystemUtils.deleteRecursivelyIfExists(projectRoot, destDir);
    }
    ProjectFilesystemUtils.mkdirs(projectRoot, destDir);

    ProjectFilesystemUtils.walkRelativeFileTree(
        projectRoot,
        srcDir,
        ProjectFilesystemUtils.getDefaultVisitOptions(),
        new SimpleFileVisitor<>() {
          @Override
          public FileVisitResult visitFile(Path srcPath, BasicFileAttributes attributes)
              throws IOException {
            if (pred.test(srcPath)) {
              Path destPath = destDir.resolve(srcDir.relativize(srcPath));
              ProjectFilesystemUtils.createParentDirs(projectRoot, destPath);
              ProjectFilesystemUtils.copy(projectRoot, srcPath, destPath, CopySourceMode.FILE);
            }
            return FileVisitResult.CONTINUE;
          }
        },
        ignoreFilter);
  }
}
