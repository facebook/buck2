/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.zip;

import static com.google.common.base.Preconditions.checkState;
import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.toList;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.jvm.java.RemoveClassesPatternsMatcher;
import com.facebook.buck.util.PatternsMatcher;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;
import java.util.regex.Pattern;
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
    System.exit(run(args));
  }

  /** Prevent {@link System#exit(int)} execution so it can be used in unit tests. */
  @VisibleForTesting
  static int run(String... args) throws IOException {
    JarBuilderMain jarBuilderMain = new JarBuilderMain();
    CmdLineParser parser = new CmdLineParser(jarBuilderMain);
    try {
      parser.parseArgument(args);
      jarBuilderMain.build();
    } catch (CmdLineException e) {
      System.err.println(e.getMessage());
      parser.printUsage(System.err);
      return 1;
    }
    return 0;
  }

  private void build() throws IOException {

    final AbsPath root = AbsPath.of(Paths.get(".").normalize().toAbsolutePath());
    final Path outputFile = root.resolve(output).getPath();
    final List<AbsPath> entriesToJar = readEntriesToJar(root, entriesToJarFilePath);
    final List<AbsPath> overrideEntriesToJar = readEntriesToJar(root, overrideEntriesToJarFilePath);
    final JarBuilder jarBuilder = concatJars ? new ConcatJarBuilder() : new JarBuilder();

    jarBuilder
        .setMainClass(mainClass)
        .setManifestFiles(getManifestFiles(root, manifestFiles))
        .setShouldMergeManifests(mergeManifests)
        .setShouldHashEntries(hashEntries)
        .setAppendJar(ofNullable(appendJar).map(Path::of).orElse(null))
        .setRemoveEntryPredicate(getRemoveEntryPredicate())
        .setEntriesToJar(getEntriesToJar(entriesToJar, concatJars))
        .setOverrideEntriesToJar(overrideEntriesToJar)
        .createJarFile(outputFile);
  }

  private List<AbsPath> getEntriesToJar(List<AbsPath> entriesToJar, boolean concatJars) {
    if (!concatJars) {
      return entriesToJar;
    }
    return new DirectoryJarBuilder(
            () -> new JarBuilder().setRemoveEntryPredicate(getRemoveEntryPredicate()))
        .buildJars(entriesToJar)
        .join();
  }

  private Predicate<? super CustomZipEntry> getRemoveEntryPredicate() {
    if (blocklistPatternsFilePath == null) {
      return null;
    }
    checkState(
        blocklistPatternsMatcher != null,
        "Must specify `--blocklist-patterns-matcher` if `--blocklist-patterns` is set!");
    try {
      final ImmutableSet<Pattern> blocklistPatterns =
          readNonEmptyLines(blocklistPatternsFilePath)
              .map(Pattern::compile)
              .collect(ImmutableSet.toImmutableSet());

      if ("substring".equals(blocklistPatternsMatcher)) {
        return entry -> new PatternsMatcher(blocklistPatterns).substringMatches(entry.getName());
      } else if ("remove_classes_patterns_matcher".equals(blocklistPatternsMatcher)) {
        return new RemoveClassesPatternsMatcher(blocklistPatterns, true);
      }
      throw new RuntimeException(
          "`--blocklist-patterns-matcher` must be either `substring` or"
              + " `remove_classes_patterns_matcher`");
    } catch (IOException e) {
      throw new RuntimeException(
          String.format("failed to read blocklistPatternsFilePath: %s", blocklistPatternsFilePath),
          e);
    }
  }

  private List<Path> getManifestFiles(final AbsPath root, final List<String> files) {
    return files.stream().map(file -> root.resolve(file).getPath()).collect(toList());
  }

  private List<AbsPath> readEntriesToJar(final AbsPath root, final String path) throws IOException {
    if (path == null) {
      return List.of();
    }
    return readNonEmptyLines(path).map(root::resolve).collect(toList());
  }

  private Stream<String> readNonEmptyLines(final String path) throws IOException {
    return Files.readAllLines(Paths.get(path)).stream()
        .map(String::trim)
        .filter(Predicate.not(String::isEmpty));
  }
}
