/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util;

import java.nio.file.Path;
import java.nio.file.Paths;

public class BuckConstant {

  public static final String DEFAULT_BUCK_OUT_DIR_NAME = "buck-out";
  private static final Path BUCK_OUTPUT_PATH_DEFAULT =
      Paths.get(System.getProperty("buck.base_buck_out_dir", DEFAULT_BUCK_OUT_DIR_NAME));

  private BuckConstant() {}

  /** The relative path to the directory where Buck will generate its files. */
  public static Path getBuckOutputPath() {
    return BUCK_OUTPUT_PATH_DEFAULT;
  }
}
