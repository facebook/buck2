/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.zip.collect;

/** Action to take when a duplicate entry is encountered when building a zip file. */
public enum OnDuplicateEntry {
  /** Fail fast. */
  FAIL,
  /** An entry is overwritten. Last entry wins. */
  OVERWRITE,
  /** All entries are stored in the archive including the duplicates. */
  APPEND,
}
