/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.abtesting.noop

import com.facebook.buck.jvm.kotlin.abtesting.ExperimentConfig
import com.facebook.buck.jvm.kotlin.abtesting.ExperimentConfigService

class NoopExperimentConfigService : ExperimentConfigService {
  override fun loadConfig(universeName: String): ExperimentConfig = NoopExperimentConfig
}
