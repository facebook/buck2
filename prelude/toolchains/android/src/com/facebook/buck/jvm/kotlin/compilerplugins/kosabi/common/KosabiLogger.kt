/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.common

import java.io.File

object Logger {
  var isEnabled: Boolean = false
    private set

  var userDefinedPath: String? = null
    set(value: String?) {
      field = value
      isEnabled = if (value != null && value.isNotEmpty()) File(value).isValidPath() else false
      // This will create an empty file if log is enabled. (for unit test purpose in case logger
      // doesn't log anything)
      if (isEnabled && value != null) {

        File(value).appendText("")
      }
    }

  fun log(msg: Any?) {
    if (!isEnabled) {
      // Do nothing if logger is disabled
      return
    }
    userDefinedPath?.let {
      val file = File(it)
      file.appendText("${msg}\n", Charsets.UTF_8)
    }
  }
}

private fun File.isValidPath(): Boolean = canonicalPath.isNotEmpty() && !isDirectory
