/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.io.file;

import com.facebook.buck.core.filesystems.RelPath;
import com.google.common.io.Files;
import java.util.Objects;

/** Matcher that matches file paths with specific extension. */
public class FileExtensionMatcher implements PathMatcher {

  private final String extension;

  private FileExtensionMatcher(String extension) {
    this.extension = extension;
  }

  @Override
  public boolean equals(Object other) {
    if (this == other) {
      return true;
    }
    if (!(other instanceof FileExtensionMatcher)) {
      return false;
    }
    FileExtensionMatcher that = (FileExtensionMatcher) other;
    return Objects.equals(extension, that.extension);
  }

  @Override
  public int hashCode() {
    return Objects.hash(extension);
  }

  @Override
  public String toString() {
    return String.format("%s extension=%s", super.toString(), extension);
  }

  @Override
  public boolean matches(RelPath path) {
    return extension.equals(Files.getFileExtension(path.toString()));
  }

  public String getExtension() {
    return extension;
  }

  /**
   * @return The matcher for paths that have {@code extension} as an extension.
   */
  public static FileExtensionMatcher of(String extension) {
    return new FileExtensionMatcher(extension);
  }
}
