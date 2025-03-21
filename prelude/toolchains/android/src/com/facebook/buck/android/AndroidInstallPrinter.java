/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android;

/** Interface for logging android installs in the AdbHelper */
public interface AndroidInstallPrinter {
  void printMessage(String message);

  void printSuccess(String message);

  void printWarning(String message);

  void printError(String message);
}
