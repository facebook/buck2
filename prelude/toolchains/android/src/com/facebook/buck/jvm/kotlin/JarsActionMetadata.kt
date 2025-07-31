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
import kotlin.collections.Map
import kotlin.collections.component1
import kotlin.collections.component2

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

  fun hasClasspathChanged(): Boolean {
    if (currentJarsDigest.size != previousJarsDigest.size) {
      return true
    }

    return currentJarsDigest.any { (path, digest) ->
      !previousJarsDigest.contains(path) || digest != previousJarsDigest.getValue(path)
    }
  }

  companion object {
    private val JAR_PATH_MATCHER = FileExtensionMatcher.of("jar")
  }
}
