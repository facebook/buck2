/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.core.build.execution.context

import com.facebook.buck.core.filesystems.AbsPath
import com.facebook.buck.util.Ansi
import com.facebook.buck.util.ClassLoaderCache
import com.facebook.buck.util.Console
import com.facebook.buck.util.ProcessExecutor
import com.facebook.buck.util.Verbosity
import com.google.common.collect.ImmutableMap
import com.google.common.io.Closer
import java.io.Closeable
import java.io.IOException
import java.io.PrintStream
import java.util.Optional

/** The context exposed for executing `IsolatedStep`s */
data class IsolatedExecutionContext(
    val classLoaderCache: ClassLoaderCache,
    val console: Console,
    val processExecutor: ProcessExecutor,
    val ruleCellRoot: AbsPath,
    val environment: ImmutableMap<String?, String?>
) : Closeable {
  val verbosity: Verbosity
    get() = console.verbosity

  val stdErr: PrintStream
    get() = console.stdErr

  val stdOut: PrintStream
    get() = console.stdErr

  val ansi: Ansi
    get() = console.ansi

  @Throws(IOException::class)
  override fun close() {
    // Using a Closer makes it easy to ensure that exceptions from one of the closeables don't
    // cancel the others.
    Closer.create().use { closer -> registerCloseables(closer) }
  }

  protected fun registerCloseables(closer: Closer) {
    closer.register(Closeable { classLoaderCache.close() })
  }

  /** Creates SubContext */
  fun createSubContext(
      newStdout: PrintStream?,
      newStderr: PrintStream?,
      verbosityOverride: Optional<Verbosity?>
  ): IsolatedExecutionContext {
    val console = this.console
    val newConsole =
        Console(verbosityOverride.orElse(console.verbosity), newStdout, newStderr, console.ansi)

    // This should replace (or otherwise retain) all of the closeable parts of the context.
    return IsolatedExecutionContext(
        classLoaderCache.addRef(),
        newConsole,
        processExecutor.cloneWithOutputStreams(newStdout, newStderr),
        ruleCellRoot,
        environment)
  }

  companion object {
    /** Returns an [IsolatedExecutionContext]. */
    @JvmStatic
    fun of(
        classLoaderCache: ClassLoaderCache,
        console: Console,
        processExecutor: ProcessExecutor,
        ruleCellRoot: AbsPath
    ): IsolatedExecutionContext {
      return IsolatedExecutionContext(
          classLoaderCache.addRef(), console, processExecutor, ruleCellRoot, ImmutableMap.of())
    }
  }
}
