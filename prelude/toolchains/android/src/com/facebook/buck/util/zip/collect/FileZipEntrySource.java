/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.zip.collect;

import java.nio.file.Path;
import java.util.Objects;

/** A file source for an entry in a zip file. */
public class FileZipEntrySource implements ZipEntrySource {

  private final Path sourceFilePath;
  private final String entryName;

  public FileZipEntrySource(Path sourceFilePath, String entryName) {
    this.sourceFilePath = sourceFilePath;
    this.entryName = entryName;
  }

  @Override
  public Path getSourceFilePath() {
    return sourceFilePath;
  }

  @Override
  public String getEntryName() {
    return entryName;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FileZipEntrySource that = (FileZipEntrySource) o;
    return Objects.equals(sourceFilePath, that.sourceFilePath)
        && Objects.equals(entryName, that.entryName);
  }

  @Override
  public int hashCode() {
    return Objects.hash(sourceFilePath, entryName);
  }
}
