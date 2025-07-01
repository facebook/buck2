/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.buildtools.snapshot

import java.nio.file.Path
import org.kohsuke.args4j.CmdLineException
import org.kohsuke.args4j.CmdLineParser
import org.kohsuke.args4j.Option

object Main {
  @Option(name = "--input-jar", usage = "Path to input jar", required = true)
  private lateinit var inputJar: Path
  @Option(name = "--output-snapshot", usage = "Path to write output snapshot", required = true)
  private lateinit var outputSnapshot: Path
  @Option(name = "--granularity", usage = "Granulairyt of snapshot", required = true)
  private lateinit var granularity: SnapshotGranularity

  @JvmStatic
  fun main(args: Array<String>) {
    val parser = CmdLineParser(this)
    try {
      parser.parseArgument(*args)
    } catch (e: CmdLineException) {
      System.err.println(e.message.toString())
      parser.printUsage(System.err)
      throw e
    }

    val generator = ClasspathSnapshotGenerator(inputJar, outputSnapshot, granularity)
    generator.run()
  }
}
