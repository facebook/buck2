/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util.unarchive;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.util.PatternsMatcher;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Optional;

/** Simple interface to extract archives of varying types */
public abstract class Unarchiver {

  /**
   * Extract a given archive to a destination
   *
   * @param archiveFile The path to the archive
   * @param extractedPath The filesystem's path that will be extracted into
   * @param relativePath The path relative to the filesystem to extract files into
   * @param stripPrefix If provided, only files under this prefix will be extracted. This prefix
   *     prefix will also be removed from the destination path. e.g. foo.tar.gz/foo/bar/baz with a
   *     prefix of foo will extract bar/baz into the destination directory. If not provided, no
   *     stripping is done.
   * @param entriesToExclude Entries that match this matcher will not be extracted
   * @param existingFileMode How to handle existing files
   * @return A list of paths to files that were created (not directories)
   * @throws IOException If the archive could not be extracted for any reason
   */
  public abstract ImmutableSet<Path> extractArchive(
      Path archiveFile,
      AbsPath extractedPath,
      Path relativePath,
      Optional<Path> stripPrefix,
      PatternsMatcher entriesToExclude,
      ExistingFileMode existingFileMode)
      throws IOException;

  /**
   * Extract a given archive to a specific directory
   *
   * @param extractedPath The filesystem's path that will be extracted into
   * @param archiveFile The path to the archive
   * @param destination The destination directory where the archive should be extracted to
   * @param existingFileMode How to handle existing files
   * @return A list of paths to files that were created (not directories)
   * @throws IOException If the archive could not be extracted for any reason
   */
  public final ImmutableList<Path> extractArchive(
      AbsPath extractedPath, Path archiveFile, Path destination, ExistingFileMode existingFileMode)
      throws IOException {
    return extractArchive(
            archiveFile,
            extractedPath,
            destination,
            Optional.empty(),
            PatternsMatcher.NONE,
            existingFileMode)
        .asList();
  }
}
