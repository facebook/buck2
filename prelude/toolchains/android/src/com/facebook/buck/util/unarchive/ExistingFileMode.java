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

/** How existing files should be handled in a destination */
public enum ExistingFileMode {
  /**
   * Just overwrite existing files. If a file is in the destination that is not in the archive, it
   * will not be removed
   */
  OVERWRITE,
  /**
   * Overwrite existing files, and make sure that directories do not have any extra files that are
   * not in the archive
   */
  OVERWRITE_AND_CLEAN_DIRECTORIES,
}
