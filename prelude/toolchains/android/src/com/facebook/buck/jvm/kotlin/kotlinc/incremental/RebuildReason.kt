/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.kotlinc.incremental

enum class RebuildReason(val message: String) {
  NO_LAST_BUILD_CONFIGURATION("Last build configuration not found"),
  BUILD_CONFIGURATION_CHANGED("Build configuration has been changed"),
  NO_LAST_KOTLIN_USED_CLASSES_FILE("kotlin-used-classes.json not found"),
  NO_JVM_ABI_WORKING_DIR("jvm_abi_gen_working_dir not found")
}
