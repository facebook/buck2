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

class SnapshotsActionMetadata(actionMetaData: ActionMetadata) {

  val previousSnapshotsDigest: Map<Path, String> =
      actionMetaData.previousDigest.filter { (path, _) ->
        val relPath = RelPath.of(path)
        SNAPSHOT_PATH_MATCHER.matches(relPath)
      }

  val currentSnapshotsDigest: Map<Path, String> =
      actionMetaData.currentDigest.filter { (path, _) ->
        val relPath = RelPath.of(path)
        SNAPSHOT_PATH_MATCHER.matches(relPath)
      }

  fun hasClasspathChanged(): Boolean {
    if (currentSnapshotsDigest.size != previousSnapshotsDigest.size) {
      return true
    }

    return currentSnapshotsDigest.any { (path, digest) ->
      !previousSnapshotsDigest.contains(path) || digest != previousSnapshotsDigest.getValue(path)
    }
  }

  /**
   * Returns true if any classpath entries were removed (present in previous but not in current).
   * This is important because the Kotlin incremental compiler doesn't reliably handle classpath
   * removals - it may fail to detect that existing compiled code references classes from the
   * removed dependency.
   */
  fun hasClasspathRemoval(): Boolean {
    return previousSnapshotsDigest.keys.any { path -> !currentSnapshotsDigest.containsKey(path) }
  }

  companion object {
    private val SNAPSHOT_PATH_MATCHER = FileExtensionMatcher.of("bin")
  }
}
