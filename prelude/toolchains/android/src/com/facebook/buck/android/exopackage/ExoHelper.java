/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.exopackage;

import com.google.common.collect.ImmutableMap;
import java.io.IOException;
import java.nio.file.Path;

/** An exo installer helper. */
interface ExoHelper {
  /** Returns the files to install. */
  ImmutableMap<Path, Path> getFilesToInstall() throws IOException;

  /** Returns metadata to install. */
  ImmutableMap<Path, String> getMetadataToInstall() throws IOException;

  /** Returns the type of this installer. */
  String getType();
}
