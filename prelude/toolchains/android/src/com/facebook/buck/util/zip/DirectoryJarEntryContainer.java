/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.zip;

import com.facebook.buck.io.file.MorePaths;
import com.facebook.buck.io.pathformat.PathFormatter;
import com.facebook.buck.util.function.ThrowingSupplier;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.Objects;
import java.util.jar.Manifest;
import java.util.stream.Stream;
import javax.annotation.Nullable;

/**
 * Provides all files and directories recursively contained in a given directory as entries to be
 * added to a jar.
 */
class DirectoryJarEntryContainer implements JarEntryContainer {

  private final Path directory;
  private final String owner;
  private Stream<Path> directoryPathStream;

  public DirectoryJarEntryContainer(Path directory) {
    this.directory = directory;
    this.owner = directory.toString();
  }

  @Nullable
  @Override
  public Manifest getManifest() throws IOException {
    Path manifestPath = directory.resolve("META-INF").resolve("MANIFEST.MF");

    if (!Files.isRegularFile(manifestPath)) {
      return null;
    }

    try (InputStream manifestStream = Files.newInputStream(manifestPath)) {
      return new Manifest(manifestStream);
    }
  }

  @Override
  public Stream<JarEntrySupplier> stream() throws IOException {
    this.directoryPathStream = Files.walk(directory, FileVisitOption.FOLLOW_LINKS);
    return directoryPathStream
        .map(
            path -> {
              String relativePath =
                  PathFormatter.pathWithUnixSeparators(MorePaths.relativize(directory, path));

              if (relativePath.isEmpty()) {
                return null;
              }

              ThrowingSupplier<InputStream, IOException> inputStreamSupplier;
              if (Files.isDirectory(path)) {
                relativePath += '/';
                inputStreamSupplier = () -> null;
              } else {
                inputStreamSupplier = () -> Files.newInputStream(path);
              }

              return new JarEntrySupplier(
                  new CustomZipEntry(relativePath), owner, inputStreamSupplier);
            })
        .filter(Objects::nonNull)
        .sorted(Comparator.comparing(entrySupplier -> entrySupplier.getEntry().getName()));
  }

  @Override
  public void close() {
    if (directoryPathStream != null) {
      directoryPathStream.close();
    }
  }
}
