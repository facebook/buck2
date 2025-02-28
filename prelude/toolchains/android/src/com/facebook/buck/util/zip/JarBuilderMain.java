/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.zip;

import static java.util.Optional.ofNullable;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.jvm.java.RemoveClassesPatternsMatcher;
import com.facebook.buck.util.PatternsMatcher;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/** Main entry point for executing {@link JarBuilder} calls. */
public class JarBuilderMain {

  @Option(name = "--output", required = true)
  private String output;

  @Option(name = "--entries-to-jar", required = true)
  private String entriesToJarFilePath;

  @Option(name = "--override-entries-to-jar")
  private String overrideEntriesToJarFilePath;

  @Option(name = "--blocklist-patterns")
  private String blocklistPatternsFilePath;

  @Option(name = "--blocklist-patterns-matcher")
  private String blocklistPatternsMatcher;

  @Option(name = "--main-class")
  private String mainClass;

  @Option(name = "--manifest-file")
  private List<String> manifestFiles = new ArrayList<>();

  @Option(name = "--merge-manifests")
  private boolean mergeManifests = false;

  @Option(name = "--hash-entries")
  private boolean hashEntries = false;

  @Option(name = "--append-jar")
  private String appendJar;

  @Option(name = "--concat-jars")
  private boolean concatJars = false;

  public static void main(String[] args) throws IOException {
    JarBuilderMain main = new JarBuilderMain();
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
    final AbsPath root = AbsPath.of(Paths.get(".").normalize().toAbsolutePath());
    final Stream<AbsPath> entriesToJar =
        Files.readAllLines(Paths.get(entriesToJarFilePath)).stream()
            .map(path -> root.resolve(Paths.get(path)));

    final Stream<AbsPath> overrideEntriesToJar =
        overrideEntriesToJarFilePath != null
            ? Files.readAllLines(Paths.get(overrideEntriesToJarFilePath)).stream()
                .map(path -> root.resolve(Paths.get(path)))
            : Stream.empty();

    final JarBuilder jarBuilder = concatJars ? new ConcatJarBuilder() : new JarBuilder();

    jarBuilder
        .setEntriesToJar(entriesToJar)
        .setOverrideEntriesToJar(overrideEntriesToJar)
        .setMainClass(mainClass)
        .setManifestFiles(
            manifestFiles.stream()
                .map(file -> root.resolve(file).getPath())
                .collect(Collectors.toList()))
        .setShouldMergeManifests(mergeManifests)
        .setShouldHashEntries(hashEntries)
        .setAppendJar(ofNullable(appendJar).map(Path::of).orElse(null));

    if (blocklistPatternsFilePath != null) {
      ImmutableSet<Pattern> blocklistPatterns =
          Files.readAllLines(Paths.get(blocklistPatternsFilePath)).stream()
              .map(Pattern::compile)
              .collect(ImmutableSet.toImmutableSet());
      Preconditions.checkState(
          blocklistPatternsMatcher != null,
          "Must specify `--blocklist-patterns-matcher` if `--blocklist-patterns` is set!");
      Predicate<? super CustomZipEntry> removeEntryPredicate;
      if (blocklistPatternsMatcher.equals("substring")) {
        removeEntryPredicate =
            entry -> new PatternsMatcher(blocklistPatterns).substringMatches(entry.getName());
      } else if (blocklistPatternsMatcher.equals("remove_classes_patterns_matcher")) {
        removeEntryPredicate = new RemoveClassesPatternsMatcher(blocklistPatterns, true);
      } else {
        throw new RuntimeException(
            "`--blocklist-patterns-matcher` must be either `substring` or"
                + " `remove_classes_patterns_matcher`");
      }

      jarBuilder.setRemoveEntryPredicate(removeEntryPredicate);
    }

    jarBuilder.createJarFile(root.resolve(output).getPath());
  }
}
