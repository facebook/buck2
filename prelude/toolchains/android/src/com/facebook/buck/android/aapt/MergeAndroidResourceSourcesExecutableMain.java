/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.aapt;

import com.android.ide.common.resources.MergingException;
import com.android.utils.StdLogger;
import com.google.common.collect.ImmutableList;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/** Main entry point for executing {@link MergeAndroidResourceSourcesUtils} calls. */
public class MergeAndroidResourceSourcesExecutableMain {

  @Option(name = "--resource-paths", required = true)
  private String resourcePathsList;

  @Option(name = "--output", required = true)
  private Path output;

  public static void main(String[] args) throws IOException {
    MergeAndroidResourceSourcesExecutableMain main =
        new MergeAndroidResourceSourcesExecutableMain();
    CmdLineParser parser = new CmdLineParser(main);
    try {
      parser.parseArgument(args);
      main.run();
      System.exit(0);
    } catch (CmdLineException e) {
      System.err.println(e.getMessage());
      parser.printUsage(System.err);
      System.exit(1);
    }
  }

  private void run() throws IOException {
    try {
      ImmutableList<Path> resPaths =
          Files.readAllLines(Paths.get(resourcePathsList)).stream()
              .map(Paths::get)
              .collect(ImmutableList.toImmutableList());
      Files.createDirectories(output);
      MergeAndroidResourceSourcesUtils.mergeResPaths(
          resPaths,
          output,
          Files.createTempDirectory("merge_android_resource_sources_temp"),
          new StdLogger(StdLogger.Level.WARNING));
    } catch (MergingException e) {
      throw new RuntimeException(e);
    }
  }
}
