/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin;

import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.jvm.kotlin.buildtools.BuildToolsKotlinc;
import com.facebook.buck.jvm.kotlin.kotlinc.Kotlinc;

public class KotlincFactory {

  private static final Logger LOG = Logger.get(KotlincFactory.class);

  public static Kotlinc create() {
    LOG.info("Kotlinc implementation used: " + BuildToolsKotlinc.class.getSimpleName());
    return new BuildToolsKotlinc();
  }
}
