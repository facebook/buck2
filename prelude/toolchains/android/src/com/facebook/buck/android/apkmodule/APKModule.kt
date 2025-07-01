/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.apkmodule

data class APKModule(val name: String) : Comparable<APKModule> {
  val isRootModule: Boolean
    get() = isRootModule(name)

  val canaryClassName: String
    get() =
        if (isRootModule) {
          "secondary"
        } else {
          String.format("store%04x", name.hashCode() and 0xFFFF)
        }

  override fun compareTo(o: APKModule): Int {
    if (this == o) {
      return 0
    }

    return name.compareTo(o.name)
  }

  companion object {
    const val ROOT_APKMODULE_NAME: String = "dex"

    @JvmStatic
    fun isRootModule(moduleName: String): Boolean {
      return moduleName == ROOT_APKMODULE_NAME
    }
  }
}
