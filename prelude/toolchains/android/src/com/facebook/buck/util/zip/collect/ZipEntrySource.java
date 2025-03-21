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

/** Represents source for an entry in a zip file. */
public interface ZipEntrySource {

  /**
   * Location of a file where source is located. Data for the entry can be located in a part of a
   * file.
   */
  Path getSourceFilePath();

  /** The name of the destination entry. */
  String getEntryName();
}
