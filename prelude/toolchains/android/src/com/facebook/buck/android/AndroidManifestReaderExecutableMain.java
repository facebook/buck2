/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/** Main entry point for executing {@link AndroidManifestReader} calls. */
public class AndroidManifestReaderExecutableMain {

  @Option(name = "--manifest-path", required = true)
  private String manifestPath;

  @Option(name = "--target-package-output")
  private String targetPackageOutputPath;

  @Option(name = "--package-output")
  private String packageOutputPath;

  @Option(name = "--instrumentation-test-runner-output")
  private String instrumentationTestRunnerOutputPath;

  public static void main(String[] args) throws IOException {
    AndroidManifestReaderExecutableMain main = new AndroidManifestReaderExecutableMain();
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
    AndroidManifestReader androidManifestReader =
        DefaultAndroidManifestReader.forPath(Paths.get(manifestPath));

    if (targetPackageOutputPath != null) {
      Files.write(
          Paths.get(targetPackageOutputPath), androidManifestReader.getTargetPackage().getBytes());
    }

    if (packageOutputPath != null) {
      Files.write(Paths.get(packageOutputPath), androidManifestReader.getPackage().getBytes());
    }

    if (instrumentationTestRunnerOutputPath != null) {
      Files.write(
          Paths.get(instrumentationTestRunnerOutputPath),
          androidManifestReader.getInstrumentationTestRunner().getBytes());
    }

    System.exit(0);
  }
}
