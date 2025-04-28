/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.manifest;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/**
 * Main entry point for executing {@link GenerateManifest#replaceApplicationIdPlaceholders(String,
 * boolean)} calls.
 */
public class ReplaceApplicationIdPlaceholdersExecutableMain {
  @Option(name = "--manifest", required = true)
  private String manifest;

  @Option(name = "--output", required = true)
  private String output;

  @Option(name = "--sanity-check-placeholders")
  private boolean runSanityCheck;

  public static void main(String[] args) throws IOException {
    ReplaceApplicationIdPlaceholdersExecutableMain main =
        new ReplaceApplicationIdPlaceholdersExecutableMain();
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
    String currentManifest = Files.readString(Paths.get(manifest));
    String updatedManifest =
        GenerateManifest.replaceApplicationIdPlaceholders(currentManifest, runSanityCheck);
    Files.writeString(Paths.get(output), updatedManifest);
  }
}
