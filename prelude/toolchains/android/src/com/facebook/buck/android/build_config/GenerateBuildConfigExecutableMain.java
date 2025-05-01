/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.build_config;

import com.facebook.buck.util.ThrowingPrintWriter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/** Main entry point for generating BuildConfig.java. */
public class GenerateBuildConfigExecutableMain {
  @Option(name = "--source", required = true)
  private String source;

  @Option(name = "--java-package", required = true)
  private String javaPackage;

  @Option(name = "--use-constant-expressions", required = true)
  private String useConstantExpressions;

  @Option(name = "--default-values-file", required = true)
  private String defaultValuesFile;

  @Option(name = "--values-file")
  private String valuesFile;

  @Option(name = "--output", required = true)
  private String output;

  public static void main(String[] args) throws IOException {
    GenerateBuildConfigExecutableMain main = new GenerateBuildConfigExecutableMain();
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
    Path defaultValuesPath = Paths.get(defaultValuesFile);
    Path outputPath = Paths.get(output);

    BuildConfigFields defaultValues =
        BuildConfigFields.fromFieldDeclarations(Files.readAllLines(defaultValuesPath));

    BuildConfigFields fields;
    if (valuesFile == null) {
      fields = defaultValues;
    } else {
      Path valuesPath = Paths.get(valuesFile);
      fields =
          defaultValues.putAll(
              BuildConfigFields.fromFieldDeclarations(Files.readAllLines(valuesPath)));
    }

    String java =
        BuildConfigs.generateBuildConfigDotJava(
            source, javaPackage, Boolean.parseBoolean(useConstantExpressions), fields);

    try (ThrowingPrintWriter writer =
        new ThrowingPrintWriter(new FileOutputStream(outputPath.toFile()))) {
      writer.printf(java);
    }

    System.exit(0);
  }
}
