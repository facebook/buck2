/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin

import com.facebook.buck.core.filesystems.RelPath
import com.facebook.buck.io.file.FileExtensionMatcher
import com.facebook.buck.jvm.java.ActionMetadata
import java.nio.file.Path

class JarsActionMetadata(actionMetaData: ActionMetadata) {

  val previousJarsDigest: Map<Path, String> =
      actionMetaData.previousDigest.filter { (path, _) ->
        val relPath = RelPath.of(path)
        JAR_PATH_MATCHER.matches(relPath)
      }

  val currentJarsDigest: Map<Path, String> =
      actionMetaData.currentDigest.filter { (path, _) ->
        val relPath = RelPath.of(path)
        JAR_PATH_MATCHER.matches(relPath)
      }

  fun hasChanged(relPath: RelPath): Boolean {
    require(JAR_PATH_MATCHER.matches(relPath))

    return previousJarsDigest[relPath.path] != currentJarsDigest[relPath.path]
  }

  companion object {
    private val JAR_PATH_MATCHER = FileExtensionMatcher.of("jar")
  }
}
