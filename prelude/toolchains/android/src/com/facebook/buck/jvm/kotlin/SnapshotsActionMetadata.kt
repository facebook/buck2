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
import java.nio.file.Paths
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
   * Returns true if classpath entries were removed. Normalizes paths by stripping content-based
   * hash segments to avoid false positives from Buck2's content-based path hashing.
   */
  fun hasClasspathRemoval(): Boolean {
    val currentNormalized =
        currentSnapshotsDigest.keys.mapTo(HashSet()) { it.stripContentHashSegments() }
    return previousSnapshotsDigest.keys.any { path ->
      !currentNormalized.contains(path.stripContentHashSegments())
    }
  }

  companion object {
    private val SNAPSHOT_PATH_MATCHER = FileExtensionMatcher.of("bin")
    private val CONTENT_HASH_PATTERN = Regex("^[0-9a-f]{16}$")

    /** Removes 16-char hex hash segments that Buck2 inserts for content-based paths. */
    internal fun Path.stripContentHashSegments(): Path {
      val segments = (0 until nameCount).map { getName(it).toString() }
      val filtered = segments.filterNot { CONTENT_HASH_PATTERN.matches(it) }
      if (filtered.size == segments.size || filtered.isEmpty()) return this
      return Paths.get(filtered[0], *filtered.subList(1, filtered.size).toTypedArray())
    }
  }
}
