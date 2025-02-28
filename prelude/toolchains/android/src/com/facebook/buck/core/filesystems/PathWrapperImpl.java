/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.core.filesystems;

import java.nio.file.Path;

/** Base implementation of two path wrappers. Not visible to users. */
abstract class PathWrapperImpl implements PathWrapper {
  protected final Path path;

  protected PathWrapperImpl(Path path) {
    this.path = path;
  }

  @Override
  public Path getPath() {
    return path;
  }

  @Override
  public String toString() {
    return path.toString();
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null || this.getClass() != obj.getClass()) {
      return false;
    }
    return this.path.equals(((PathWrapperImpl) obj).path);
  }

  @Override
  public int hashCode() {
    return path.hashCode();
  }
}
