/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java

import com.facebook.buck.core.filesystems.AbsPath
import com.facebook.buck.core.filesystems.RelPath
import com.facebook.buck.util.ClassLoaderCache
import com.facebook.buck.util.ProcessExecutor
import com.facebook.buck.util.Verbosity
import com.google.common.collect.ImmutableMap
import java.io.PrintStream

data class JavacExecutionContext(
    val stdErr: PrintStream,
    val classLoaderCache: ClassLoaderCache,
    val verbosity: Verbosity,
    val ruleCellRoot: AbsPath,
    val environment: ImmutableMap<String, String>,
    val processExecutor: ProcessExecutor,
    val configuredBuckOut: RelPath
)
