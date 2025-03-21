/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd.command

import com.facebook.buck.cd.model.java.BuildMode as ProtoBuildMode

enum class BuildMode {
  LIBRARY,
  ABI;

  companion object {
    fun fromProto(model: ProtoBuildMode): BuildMode {
      return when (model) {
        ProtoBuildMode.LIBRARY -> LIBRARY
        ProtoBuildMode.ABI -> ABI
        ProtoBuildMode.BUILD_MODE_UNSPECIFIED,
        ProtoBuildMode.UNRECOGNIZED -> error("${model} is not supported")
      }
    }
  }
}
