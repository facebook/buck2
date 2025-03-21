/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.kotlinc.incremental

import java.nio.file.Path

/** A hierarchy representing source files changes for incremental compilation */
sealed interface KotlinSourceChanges {
  /**
   * A marker object stating that the API consumer is not capable of calculating source file
   * changes.
   */
  data object ToBeCalculated : KotlinSourceChanges

  /**
   * A class containing [modifiedFiles] and [removedFiles] calculated from source file changes by
   * the API consumer.
   */
  data class Known(val addedAndModifiedFiles: List<Path>, val removedFiles: List<Path>) :
      KotlinSourceChanges
}
