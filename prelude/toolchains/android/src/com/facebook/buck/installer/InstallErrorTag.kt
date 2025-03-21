/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.installer

interface InstallErrorTag {
  fun getErrorCategory(): InstallErrorCategory

  fun getName(): String
}

object DefaultInstallErrorTag : InstallErrorTag {
  override fun getErrorCategory(): InstallErrorCategory = InstallErrorCategory.INFRA

  override fun getName(): String = "UNKNOWN"
}

object InfraTimeoutErrorTag : InstallErrorTag {
  override fun getErrorCategory(): InstallErrorCategory = InstallErrorCategory.INFRA

  override fun getName(): String = "Timeout"
}
