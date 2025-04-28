/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.manifest;

import com.android.common.utils.StdLogger;
import com.facebook.buck.util.ThrowingPrintWriter;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/**
 * Main entry point for executing {@link GenerateManifest} calls.
 *
 * <p>Expected usage: {@code this_binary <skeleton_manifest_path> <module_name>
 * <library_manifest_paths_file> <placeholder_entries_file> <output_path> <merge_report_path} .
 */
public class GenerateManifestExecutableMain {
  @Option(name = "--skeleton-manifest", required = true)
  private String skeletonManifest;

  @Option(name = "--module-name", required = true)
  private String moduleName;

  @Option(name = "--library-manifests-list", required = true)
  private String libraryManifestsList;

  @Option(name = "--placeholder-entries-list", required = true)
  private String placeholderEntriesList;

  @Option(name = "--output", required = true)
  private String output;

  @Option(name = "--merge-report", required = true)
  private String mergeReport;

  public static void main(String[] args) throws IOException {
    GenerateManifestExecutableMain main = new GenerateManifestExecutableMain();
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
    ImmutableList<Path> libraryManifestsFilePaths =
        Files.readAllLines(Paths.get(libraryManifestsList)).stream()
            .map(Paths::get)
            .collect(ImmutableList.toImmutableList());

    ImmutableMap<String, String> placeholderEntries =
        Files.readAllLines(Paths.get(placeholderEntriesList)).stream()
            .map(s -> s.split(" "))
            .collect(ImmutableMap.toImmutableMap(arr -> arr[0], arr -> arr[1]));

    Path outputPath = Paths.get(output);

    String xmlText =
        GenerateManifest.generateXml(
            Paths.get(skeletonManifest),
            moduleName,
            libraryManifestsFilePaths,
            placeholderEntries,
            outputPath,
            Paths.get(mergeReport),
            new StdLogger(StdLogger.Level.ERROR));

    try (ThrowingPrintWriter writer =
        new ThrowingPrintWriter(new FileOutputStream(outputPath.toFile()))) {
      writer.printf(xmlText);
    }

    System.exit(0);
  }
}
