/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util

import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableMap
import java.nio.file.Path
import java.util.Optional

/** Value type passed to [ProcessExecutor] to launch a process. */
data class ProcessExecutorParams
@JvmOverloads
constructor(
    /** The command and arguments to launch. */
    val command: ImmutableList<String>,
    /** If present, the current working directory for the launched process. */
    val environment: Optional<ImmutableMap<String, String?>> = Optional.empty(),
    /**
     * If present, the map of environment variables used for the launched process. Otherwise,
     * inherits the current process's environment.
     */
    val directory: Optional<Path> = Optional.empty()
) {

  companion object {
    @JvmStatic
    fun ofCommand(vararg args: String): ProcessExecutorParams {
      return ProcessExecutorParams(ImmutableList.copyOf(args))
    }
  }
}
