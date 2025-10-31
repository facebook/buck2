/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.kotlinc.incremental

enum class RebuildReason(val message: String) {
  NO_LAST_BUILD_CONFIGURATION("Last build configuration not found"),
  BUILD_CONFIGURATION_CHANGED("Build configuration has been changed"),
  NO_LAST_DEP_FILE("dep-file.txt not found"),
  NO_LAST_USED_JARS("used-jars.json not found"),
  NO_JVM_ABI_DIR("jvm_abi_gen_dir not found"),
}
