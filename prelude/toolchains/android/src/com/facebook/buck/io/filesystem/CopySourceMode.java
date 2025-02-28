/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.io.filesystem;

/** Controls the behavior of how the source should be treated when copying. */
public enum CopySourceMode {
  /** Copy the single source file into the destination path. */
  FILE,

  /**
   * Treat the source as a directory and copy each file inside it to the destination path, which
   * must be a directory.
   */
  DIRECTORY_CONTENTS_ONLY,

  /**
   * Treat the source as a directory. Copy the directory and its contents to the destination path,
   * which must be a directory.
   */
  DIRECTORY_AND_CONTENTS,
}
