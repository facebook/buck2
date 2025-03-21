/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.util.unarchive.ArchiveFormat;
import com.facebook.buck.util.unarchive.ExistingFileMode;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/** Utilities for handling paths to java source files. */
public class JavaPaths {

  public static final String SRC_ZIP = ".src.zip";
  public static final String SRC_JAR = "-sources.jar";

  /**
   * Processes a list of java source files, extracting and SRC_ZIP or SRC_JAR to the working
   * directory and returns a list of all the resulting .java files.
   */
  static ImmutableList<RelPath> extractArchivesAndGetPaths(
      AbsPath ruleCellPathRoot, ImmutableSet<RelPath> javaSourceFilePaths, Path workingDirectory)
      throws IOException {

    // Add sources file or sources list to command
    ImmutableList.Builder<RelPath> sources = ImmutableList.builder();
    for (RelPath path : javaSourceFilePaths) {
      String pathString = path.toString();
      if (pathString.endsWith(".java")) {
        sources.add(path);
      } else if (pathString.endsWith(SRC_ZIP) || pathString.endsWith(SRC_JAR)) {
        // For a Zip of .java files, create a JavaFileObject for each .java entry.
        ImmutableList<Path> zipPaths =
            ArchiveFormat.ZIP
                .getUnarchiver()
                .extractArchive(
                    ruleCellPathRoot,
                    ruleCellPathRoot.resolve(path).getPath(),
                    ruleCellPathRoot.resolve(workingDirectory).getPath(),
                    ExistingFileMode.OVERWRITE);
        sources.addAll(
            zipPaths.stream()
                .filter(input -> input.toString().endsWith(".java"))
                .map(ruleCellPathRoot::relativize)
                .iterator());
      }
    }
    return sources.build();
  }

  /**
   * Traverses a list of java inputs return the list of all found files (for SRC_ZIP/SRC_JAR, it
   * returns the paths from within the archive).
   */
  static ImmutableList<Path> getExpandedSourcePaths(Iterable<Path> javaSourceFilePaths)
      throws IOException {
    // Add sources file or sources list to command
    ImmutableList.Builder<Path> sources = ImmutableList.builder();
    for (Path path : javaSourceFilePaths) {
      String pathString = path.toString();
      if (pathString.endsWith(SRC_ZIP) || pathString.endsWith(SRC_JAR)) {
        try (ZipFile zipFile = new ZipFile(path.toFile())) {
          Enumeration<? extends ZipEntry> entries = zipFile.entries();
          while (entries.hasMoreElements()) {
            sources.add(Paths.get(entries.nextElement().getName()));
          }
        }
      } else {
        sources.add(path);
      }
    }
    return sources.build();
  }
}
