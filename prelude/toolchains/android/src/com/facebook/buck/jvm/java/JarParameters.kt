/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java

import com.facebook.buck.core.filesystems.RelPath
import com.google.common.collect.ImmutableSortedSet
import java.util.Optional
import java.util.function.Predicate
import java.util.logging.Level

data class JarParameters(
    val hashEntries: Boolean,
    val mergeManifests: Boolean,
    val jarPath: RelPath,
    val removeEntryPredicate: Predicate<Any>,
    val entriesToJar: ImmutableSortedSet<RelPath>,
    val overrideEntriesToJar: ImmutableSortedSet<RelPath>,
    val mainClass: Optional<String>,
    val manifestFile: Optional<RelPath>,
    val duplicatesLogLevel: Level
)
