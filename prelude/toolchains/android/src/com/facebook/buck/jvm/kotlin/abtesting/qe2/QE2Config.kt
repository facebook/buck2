/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.abtesting.qe2

import com.facebook.tools.qe2.QE2Utils
import org.json.JSONObject

class QE2Config private constructor(private val qe2Utils: QE2Utils, private val qe: JSONObject) {

  fun getBoolParam(param: String, default: Boolean): Boolean {
    return qe2Utils.getQEBoolParam(qe, param, default)
  }

  fun getIntParam(param: String, default: Int): Int {
    return qe2Utils.getQEIntParam(qe, param, default)
  }

  fun getStringParam(param: String, default: String): String {
    return qe2Utils.getQEStringParam(qe, param, default)
  }

  companion object {
    @JvmStatic
    fun create(universeName: String): QE2Config {
      val qe2Utils = QE2Utils(BuckQE2Logger)

      return QE2Config(qe2Utils, qe2Utils.genQE(universeName))
    }
  }
}
