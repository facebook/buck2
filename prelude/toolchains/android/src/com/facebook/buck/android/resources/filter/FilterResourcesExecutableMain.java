/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.resources.filter;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.io.file.GlobPatternMatcher;
import com.facebook.buck.io.file.PathMatcher;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.facebook.buck.util.json.ObjectMappers;
import com.fasterxml.jackson.core.type.TypeReference;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableBiMap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Map;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/** Entry point for filtering resources. */
public class FilterResourcesExecutableMain {
  private static final ImmutableList<PathMatcher> NON_ASSET_FILENAMES_MATCHERS =
      ImmutableList.of(GlobPatternMatcher.of("**/.DS_Store"));

  @Option(name = "--in-res-dir-to-out-res-dir-map", required = true)
  private String inResDirToOutResDirMapPath;

  @Option(name = "--voltron-in-res-dir-to-out-res-dir-map")
  private String voltronInResDirToOutResDirMapPath;

  @Option(name = "--target-densities")
  private String targetDensities;

  @Option(name = "--enable-string-as-assets-filtering")
  private boolean enableStringsAsAssetsFiltering;

  @Option(name = "--not-filtered-string-dirs")
  private String notFilteredStringDirsFile;

  @Option(name = "--string-files-list-output")
  private String stringFilesListOutput;

  @Option(name = "--locales")
  private String localesString;

  @Option(name = "--packaged-locales")
  private String packagedLocalesString;

  @Option(name = "--post-filter-resources-cmd")
  private String postFilterResourcesCmd;

  @Option(name = "--post-filter-resources-cmd-override-symbols-output")
  private String postFilterResourcesCmdOverrideSymbols;

  @Option(name = "--allowlisted-locales")
  private String allowlistedLocalesFile;

  public static void main(String[] args) throws IOException {
    FilterResourcesExecutableMain main = new FilterResourcesExecutableMain();
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
    AbsPath root = AbsPath.of(Paths.get(".").normalize().toAbsolutePath());
    Map<String, ImmutableBiMap<Path, Path>> rawMap =
        ObjectMappers.READER.readValue(
            ObjectMappers.createParser(Paths.get(inResDirToOutResDirMapPath)),
            new TypeReference<Map<String, ImmutableBiMap<Path, Path>>>() {});
    ImmutableBiMap<Path, Path> inResDirToOutResDirMap = rawMap.get("res_dir_map");
    ImmutableSet<ResourceFilters.Density> targetDensitiesSet =
        targetDensities != null
            ? Arrays.stream(targetDensities.split(","))
                .map(ResourceFilters.Density::from)
                .collect(ImmutableSet.toImmutableSet())
            : ImmutableSet.of();
    ImmutableSet<String> locales =
        localesString != null ? ImmutableSet.copyOf(localesString.split(",")) : ImmutableSet.of();
    ImmutableSet<String> packagedLocales =
        packagedLocalesString != null
            ? ImmutableSet.copyOf(packagedLocalesString.split(","))
            : ImmutableSet.of();
    ImmutableSet<Path> notFilteredStringDirs =
        notFilteredStringDirsFile != null
            ? Files.readAllLines(Paths.get(notFilteredStringDirsFile)).stream()
                .map(Paths::get)
                .collect(ImmutableSet.toImmutableSet())
            : ImmutableSet.of();

    ImmutableMap<Path, ImmutableSet<String>> allowlistedLocalesDirs =
        allowlistedLocalesFile != null
            ? ObjectMappers.READER.readValue(
                ObjectMappers.createParser(Paths.get(allowlistedLocalesFile)),
                new TypeReference<ImmutableMap<Path, ImmutableSet<String>>>() {})
            : ImmutableMap.of();

    Predicate<Path> filteringPredicate =
        FilteringPredicate.getFilteringPredicate(
            root,
            ProjectFilesystemUtils.getIgnoreFilter(root, true, NON_ASSET_FILENAMES_MATCHERS),
            inResDirToOutResDirMap,
            !targetDensitiesSet.isEmpty(),
            targetDensitiesSet,
            // TODO(T122759074) Do we need to support canDownscale or not?
            /* canDownscale */ false,
            locales,
            packagedLocales,
            enableStringsAsAssetsFiltering,
            notFilteredStringDirs,
            allowlistedLocalesDirs);

    FilteredDirectoryCopier.copyDirs(
        root,
        ProjectFilesystemUtils.getIgnoreFilter(root, true, NON_ASSET_FILENAMES_MATCHERS),
        inResDirToOutResDirMap,
        filteringPredicate);

    if (voltronInResDirToOutResDirMapPath != null) {
      Map<String, ImmutableBiMap<Path, Path>> rawVoltronMap =
          ObjectMappers.READER.readValue(
              ObjectMappers.createParser(Paths.get(voltronInResDirToOutResDirMapPath)),
              new TypeReference<Map<String, ImmutableBiMap<Path, Path>>>() {});
      ImmutableBiMap<Path, Path> voltronInResDirToOutResDirMap = rawVoltronMap.get("res_dir_map");

      FilteredDirectoryCopier.copyDirs(
          root,
          ProjectFilesystemUtils.getEmptyIgnoreFilter(),
          voltronInResDirToOutResDirMap,
          FilteringPredicate.getVoltronLanguagePackPredicate());
    }

    if (postFilterResourcesCmd != null) {
      Preconditions.checkState(
          postFilterResourcesCmdOverrideSymbols != null,
          "Must specify an override symbols file if a post-filter-resources-cmd is specified!");
      ImmutableList.Builder<String> postFilterResourcesCmdList = ImmutableList.builder();
      postFilterResourcesCmdList
          .addAll(Arrays.stream(postFilterResourcesCmd.split("\\s+")).collect(Collectors.toList()))
          .add(inResDirToOutResDirMapPath)
          .add(postFilterResourcesCmdOverrideSymbols);
      Process postFilterResourcesProcess =
          new ProcessBuilder().command(postFilterResourcesCmdList.build()).start();
      try {
        int exitCode = postFilterResourcesProcess.waitFor();
        if (exitCode != 0) {
          String error = new String(postFilterResourcesProcess.getErrorStream().readAllBytes());
          throw new RuntimeException("post_filter_resources_cmd failed with error: " + error);
        }
      } catch (InterruptedException e) {
        throw new RuntimeException(e);
      }
    }

    // We need to output a list of all the string files if and only if we are doing
    // strings-as-assets filtering.
    Preconditions.checkState(enableStringsAsAssetsFiltering == (stringFilesListOutput != null));
    if (stringFilesListOutput != null) {
      ImmutableList<Path> allStringFiles =
          GetStringsFiles.getFiles(
              root,
              ProjectFilesystemUtils.getEmptyIgnoreFilter(),
              inResDirToOutResDirMap.keySet().asList());
      Files.write(
          Paths.get(stringFilesListOutput),
          allStringFiles.stream().map(Path::toString).collect(Collectors.toList()));
    }

    System.exit(0);
  }
}
