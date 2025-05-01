/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.resources;

import com.facebook.buck.android.aapt.RDotTxtEntry;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.util.zip.JarBuilder;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.EnumSet;
import java.util.Optional;
import java.util.stream.Collectors;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/** Main entry point for executing {@link MergeAndroidResources} calls. */
public class MergeAndroidResourcesExecutableMain {
  @Option(name = "--symbol-file-info", required = true)
  private String symbolFileInfo;

  @Option(name = "--output-dir", required = true)
  private String outputDirString;

  @Option(name = "--output-dir-zipped", required = true)
  private String outputDirZippedString;

  @Option(name = "--strings-output-dir")
  private String stringsOutputDirString;

  @Option(name = "--strings-output-dir-zipped")
  private String stringsOutputDirZippedString;

  @Option(name = "--ids-output-dir")
  private String idsOutputDirString;

  @Option(name = "--ids-output-dir-zipped")
  private String idsOutputDirZippedString;

  @Option(name = "--force-final-resource-ids")
  private boolean forceFinalResourceIds = false;

  @Option(name = "--uber-r-dot-txt")
  private String uberRDotTxtFilesList;

  @Option(name = "--banned-duplicate-resource-types")
  private String bannedDuplicateResourceTypesList;

  @Option(name = "--override-symbols")
  private String overrideSymbolsList;

  @Option(name = "--duplicate-resource-allowlist-path")
  private String duplicateResourceAllowlist;

  @Option(name = "--union-package")
  private String unionPackageString;

  @Option(name = "--referenced-resources-lists")
  private String referencedResourcesLists;

  public static void main(String[] args) throws IOException {
    MergeAndroidResourcesExecutableMain main = new MergeAndroidResourcesExecutableMain();
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
    ImmutableMap.Builder<Path, String> symbolsFileToRDotJavaPackage = ImmutableMap.builder();
    ImmutableMap.Builder<Path, String> symbolsFileToTargetName = ImmutableMap.builder();

    for (String line : Files.readAllLines(Paths.get(symbolFileInfo))) {
      String[] parts = line.split(" ");
      Preconditions.checkState(parts.length == 3);
      Path symbolsFilePath = Paths.get(parts[0]);
      Path rDotJavaPackageFilePath = Paths.get(parts[1]);
      String rDotJavaPackage = new String(Files.readAllBytes(rDotJavaPackageFilePath));
      symbolsFileToRDotJavaPackage.put(symbolsFilePath, rDotJavaPackage);
      symbolsFileToTargetName.put(symbolsFilePath, parts[2]);
    }

    ImmutableList<Path> uberRDotTxt =
        uberRDotTxtFilesList != null
            ? Files.readAllLines(Paths.get(uberRDotTxtFilesList)).stream()
                .map(Paths::get)
                .collect(ImmutableList.toImmutableList())
            : ImmutableList.of();
    EnumSet<RDotTxtEntry.RType> bannedDuplicateResourceTypes =
        bannedDuplicateResourceTypesList != null
            ? EnumSet.copyOf(
                Files.readAllLines(Paths.get(bannedDuplicateResourceTypesList)).stream()
                    .map(type -> RDotTxtEntry.RType.valueOf(type.toUpperCase()))
                    .collect(Collectors.toList()))
            : EnumSet.noneOf(RDotTxtEntry.RType.class);
    ImmutableList<Path> overrideSymbols =
        overrideSymbolsList != null
            ? Files.readAllLines(Paths.get(overrideSymbolsList)).stream()
                .map(Paths::get)
                .collect(ImmutableList.toImmutableList())
            : ImmutableList.of();
    Optional<Path> duplicateResourceAllowlistPath =
        Optional.ofNullable(duplicateResourceAllowlist).map(Paths::get);
    Optional<String> unionPackage = Optional.ofNullable(unionPackageString);

    Path outputDir = Paths.get(outputDirString);
    Optional<Path> stringsOutputDirPath =
        Optional.ofNullable(stringsOutputDirString).map(Paths::get);
    Optional<Path> idsOutputDirPath = Optional.ofNullable(idsOutputDirString).map(Paths::get);

    ImmutableSet.Builder<String> referencedResources = ImmutableSet.builder();
    if (referencedResourcesLists != null) {
      for (String referencedResourcesList :
          Files.readAllLines(Paths.get(referencedResourcesLists))) {
        referencedResources.addAll(Files.readAllLines(Paths.get(referencedResourcesList)));
      }
    }

    try {
      MergeAndroidResources.mergeAndroidResources(
          uberRDotTxt,
          symbolsFileToRDotJavaPackage.build(),
          symbolsFileToTargetName.build(),
          forceFinalResourceIds,
          bannedDuplicateResourceTypes,
          duplicateResourceAllowlistPath,
          unionPackage,
          overrideSymbols,
          outputDir,
          stringsOutputDirPath,
          idsOutputDirPath,
          referencedResources.build());
    } catch (MergeAndroidResources.DuplicateResourceException e) {
      throw new RuntimeException(e);
    }

    Path outputDirZipped = Paths.get(outputDirZippedString);
    createZip(outputDir, outputDirZipped);

    Optional<Path> stringsOutputDirZippedPath =
        Optional.ofNullable(stringsOutputDirZippedString).map(Paths::get);
    Preconditions.checkState(
        stringsOutputDirPath.isPresent() == stringsOutputDirZippedPath.isPresent());
    if (stringsOutputDirZippedPath.isPresent()) {
      createZip(stringsOutputDirPath.get(), stringsOutputDirZippedPath.get());
    }

    Optional<Path> idsOutputDirZippedPath =
        Optional.ofNullable(idsOutputDirZippedString).map(Paths::get);
    Preconditions.checkState(idsOutputDirPath.isPresent() == idsOutputDirZippedPath.isPresent());
    if (idsOutputDirZippedPath.isPresent()) {
      createZip(idsOutputDirPath.get(), idsOutputDirZippedPath.get());
    }

    System.exit(0);
  }

  private void createZip(Path outputDir, Path outputDirZipped) throws IOException {
    AbsPath root = AbsPath.of(Paths.get(".").normalize().toAbsolutePath());
    new JarBuilder()
        .setEntriesToJar(ImmutableList.of(root.resolve(outputDir)))
        .createJarFile(root.resolve(outputDirZipped).getPath());
  }
}
