/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.zip;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.io.file.PathMatcher;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.facebook.buck.io.pathformat.PathFormatter;
import com.facebook.buck.util.types.Pair;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.hash.Hashing;
import com.google.common.io.ByteSource;
import com.google.common.io.ByteStreams;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.DirectoryStream;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.zip.ZipEntry;

public class Zip {
  private static final Logger LOG = Logger.get(Zip.class);

  private Zip() {}

  /**
   * Takes a sequence of paths relative to the project root and writes a zip file to {@code out}
   * with the contents and structure that matches that of the specified paths.
   */
  public static void create(AbsPath rootPath, Collection<Path> pathsToIncludeInZip, Path out)
      throws IOException {
    try (CustomZipOutputStream zip = ZipOutputStreams.newOutputStream(out)) {
      for (Path path : pathsToIncludeInZip) {

        boolean isDirectory = ProjectFilesystemUtils.isDirectory(rootPath, path);
        CustomZipEntry entry = new CustomZipEntry(path, isDirectory);

        // We want deterministic ZIPs, so avoid mtimes.
        entry.setFakeTime();

        entry.setExternalAttributes(getFileAttributesForZipEntry(rootPath, path));

        zip.putNextEntry(entry);
        if (!isDirectory) {
          try (InputStream input = ProjectFilesystemUtils.newFileInputStream(rootPath, path)) {
            ByteStreams.copy(input, zip);
          }
        }
        zip.closeEntry();
      }
    }
  }

  /** Walks the file tree rooted in baseDirectory to create zip entries */
  public static void walkBaseDirectoryToCreateEntries(
      AbsPath rootPath,
      Map<String, Pair<CustomZipEntry, Optional<Path>>> entries,
      Path baseDir,
      ImmutableSet<PathMatcher> ignoredPaths,
      ImmutableSet<Path> paths,
      boolean junkPaths,
      ZipCompressionLevel compressionLevel)
      throws IOException {
    // Since filesystem traversals can be non-deterministic, sort the entries we find into
    // a tree map before writing them out.
    FileVisitor<Path> pathFileVisitor =
        new SimpleFileVisitor<Path>() {
          private boolean isSkipFile(Path file) {
            return !paths.isEmpty() && !paths.contains(file);
          }

          private String getEntryName(Path path) {
            Path relativePath = junkPaths ? path.getFileName() : baseDir.relativize(path);
            return PathFormatter.pathWithUnixSeparators(relativePath);
          }

          private CustomZipEntry getZipEntry(String entryName, Path path, BasicFileAttributes attr)
              throws IOException {
            boolean isDirectory = ProjectFilesystemUtils.isDirectory(rootPath, path);
            if (isDirectory) {
              entryName += "/";
            }

            CustomZipEntry entry = new CustomZipEntry(entryName);
            // We want deterministic ZIPs, so avoid mtimes.
            entry.setFakeTime();
            entry.setCompressionLevel(
                isDirectory ? ZipCompressionLevel.NONE.getValue() : compressionLevel.getValue());
            // If we're using STORED files, we must manually set the CRC, size, and compressed size.
            if (entry.getMethod() == ZipEntry.STORED && !isDirectory) {
              entry.setSize(attr.size());
              entry.setCompressedSize(attr.size());
              entry.setCrc(
                  new ByteSource() {
                    @Override
                    public InputStream openStream() throws IOException {
                      return ProjectFilesystemUtils.newFileInputStream(rootPath, path);
                    }
                  }.hash(Hashing.crc32()).padToLong());
            }

            long externalAttributes = getFileAttributesForZipEntry(rootPath, path);
            LOG.verbose(
                "Setting mode for entry %s path %s to 0x%08X", entryName, path, externalAttributes);
            entry.setExternalAttributes(externalAttributes);
            return entry;
          }

          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
              throws IOException {
            if (!file.equals(baseDir) && !isSkipFile(file)) {
              CustomZipEntry entry = getZipEntry(getEntryName(file), file, attrs);
              entries.put(entry.getName(), new Pair<>(entry, Optional.of(file)));
            }
            return FileVisitResult.CONTINUE;
          }

          @Override
          public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs)
              throws IOException {
            if (!dir.equals(baseDir) && !isSkipFile(dir)) {
              CustomZipEntry entry = getZipEntry(getEntryName(dir), dir, attrs);
              entries.put(entry.getName(), new Pair<>(entry, Optional.empty()));
            }
            return FileVisitResult.CONTINUE;
          }
        };

    DirectoryStream.Filter<? super Path> ignoreFilter =
        ProjectFilesystemUtils.getIgnoreFilter(rootPath, true, ignoredPaths);
    ProjectFilesystemUtils.walkRelativeFileTree(
        rootPath,
        baseDir,
        ProjectFilesystemUtils.getDefaultVisitOptions(),
        pathFileVisitor,
        ignoreFilter);
  }

  /** Writes entries to zipOut stream. */
  public static void writeEntriesToZip(
      AbsPath rootPath,
      CustomZipOutputStream zipOut,
      Map<String, Pair<CustomZipEntry, Optional<Path>>> entries)
      throws IOException {
    // Write the entries out using the iteration order of the tree map above.
    for (Pair<CustomZipEntry, Optional<Path>> entry : entries.values()) {
      zipOut.putNextEntry(entry.getFirst());
      if (entry.getSecond().isPresent()) {
        try (InputStream input =
            ProjectFilesystemUtils.newFileInputStream(rootPath, entry.getSecond().get())) {
          ByteStreams.copy(input, zipOut);
        }
      }
      zipOut.closeEntry();
    }
  }

  /**
   * @return a list of all entry names in a zip archive.
   */
  public static ImmutableList<String> getAllZipEntries(Path archiveAbsolutePath)
      throws IOException {
    try (java.util.zip.ZipFile zip = new java.util.zip.ZipFile(archiveAbsolutePath.toFile())) {
      return Collections.list(zip.entries()).stream()
          .map(ZipEntry::getName)
          .collect(ImmutableList.toImmutableList());
    }
  }

  private static long getFileAttributesForZipEntry(AbsPath rootPath, Path path) throws IOException {
    return ProjectFilesystemUtils.getPosixFileModes(rootPath, path) << 16;
  }
}
