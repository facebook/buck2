/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.installer;

import java.nio.file.Path;

/**
 * Install Command interface. Support for a specific installer such as iOS or Android is implemented
 * as subclasses of this interface. *
 */
public interface InstallCommand {
  /** Installs an artifact from a given path/location. */
  InstallResult fileReady(String artifact, Path artifactPath, InstallId installId);

  /** Indicate that all files have been received by the installer */
  InstallResult allFilesReady(InstallId installId);
}
