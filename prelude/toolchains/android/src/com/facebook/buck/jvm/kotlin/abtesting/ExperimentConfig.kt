/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.abtesting

import com.facebook.tools.qe2.QE2Utils
import org.json.JSONObject

class ExperimentConfig
private constructor(private val qE2Utils: QE2Utils, private val qe: JSONObject) {

  fun getBoolParam(param: String, default: Boolean): Boolean {
    return qE2Utils.getQEBoolParam(qe, param, default)
  }

  fun getIntParam(param: String, default: Int): Int {
    return qE2Utils.getQEIntParam(qe, param, default)
  }

  fun getStringParam(param: String, default: String): String {
    return qE2Utils.getQEStringParam(qe, param, default)
  }

  companion object {
    @JvmStatic
    fun create(universeName: String): ExperimentConfig {
      val qE2Utils = QE2Utils(BuckQE2Logger)

      return ExperimentConfig(qE2Utils, qE2Utils.genQE(universeName))
    }
  }
}
