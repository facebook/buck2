/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.dex;

import com.android.tools.r8.CompilationFailedException;
import com.facebook.buck.util.zip.ZipScrubber;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/** Main entry point for executing {@link com.android.tools.r8.D8Command} calls. */
public class D8ExecutableMain {
  /** name suffix that identifies it as a Java class file. */
  private static final String CLASS_NAME_SUFFIX = ".class";

  @Option(name = "--output-dex-file", required = true)
  private String outputDex;

  @Option(name = "--files-to-dex-list")
  private String filesToDexList;

  @Option(name = "--file-to-dex")
  private String fileToDex;

  @Option(name = "--android-jar", required = true)
  private String androidJar;

  @Option(name = "--intermediate")
  private boolean intermediate = false;

  @Option(name = "--no-desugar")
  private boolean noDesugar = false;

  @Option(name = "--no-optimize")
  private boolean noOptimize = false;

  @Option(name = "--force-jumbo")
  private boolean forceJumbo = false;

  @Option(name = "--primary-dex-class-names-path")
  private String primaryDexClassNamesList;

  @Option(name = "--referenced-resources-path")
  private String referencedResourcesList;

  @Option(name = "--classpath-files")
  private String classpathFilesList;

  @Option(name = "--min-sdk-version")
  private String minSdkVersionString;

  @Option(name = "--weight-estimate-path")
  private String weightEstimateOutput;

  @Option(name = "--weight-factor")
  private String weightFactor;

  @Option(name = "--class-names-path")
  private String classNamesOutput;

  /**
   * When using jar compression, the secondary dex directory consists of N secondary dex jars, each
   * of which has a corresponding .meta file (the secondaryDexMetadataFile) containing a single line
   * of the form:
   *
   * <p>jar:<size of secondary dex jar (in bytes)> dex:<size of uncompressed dex file (in bytes)>
   *
   * <p>When using jar or raw compression, it also contains a metadata.txt file, which consists of N
   * lines, one for each secondary dex file. Those lines consist of:
   *
   * <p><secondary dex file name> <sha1 hash of secondary dex> <canary class>
   *
   * <p>We write the line that needs to be added to metadata.txt for this secondary dex to
   * secondaryDexMetadataLine, and we use the secondaryDexCanaryClassName for the <canary class>.
   */
  @Option(name = "--secondary-dex-compression")
  private String secondaryDexCompression;

  @Option(name = "--secondary-dex-metadata-file")
  private String secondaryDexMetadataFile;

  @Option(name = "--secondary-dex-metadata-line")
  private String secondaryDexMetadataLine;

  @Option(name = "--secondary-dex-canary-class-name")
  private String secondaryDexCanaryClassName;

  private static final String DEX_JAR_SUFFIX = ".dex.jar";

  public static void main(String[] args) throws IOException {
    D8ExecutableMain main = new D8ExecutableMain();
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
    ImmutableSet<Path> filesToDex;
    if (filesToDexList != null) {
      Preconditions.checkState(fileToDex == null);
      filesToDex =
          Files.readAllLines(Paths.get(filesToDexList)).stream()
              .map(Paths::get)
              .collect(ImmutableSet.toImmutableSet());
    } else {
      Preconditions.checkState(fileToDex != null);
      filesToDex = ImmutableSet.of(Paths.get(fileToDex));
    }

    Optional<Path> primaryDexClassNamesPath =
        Optional.ofNullable(primaryDexClassNamesList).map(Paths::get);
    Optional<Path> referencedResourcesPath =
        Optional.ofNullable(referencedResourcesList).map(Paths::get);

    ImmutableSet<Path> classpathFiles =
        classpathFilesList == null
            ? ImmutableSet.of()
            : Files.readAllLines(Paths.get(classpathFilesList)).stream()
                .map(Paths::get)
                .collect(ImmutableSet.toImmutableSet());

    Optional<Integer> minSdkVersion =
        Optional.ofNullable(minSdkVersionString).map(Integer::parseInt);
    Optional<Path> weightEstimatePath = Optional.ofNullable(weightEstimateOutput).map(Paths::get);
    Optional<Path> classNamesPath = Optional.ofNullable(classNamesOutput).map(Paths::get);

    Path outputPath = Paths.get(outputDex);
    Path d8Output =
        primaryDexClassNamesPath.isPresent() ? Files.createTempDirectory("dexTmpDir") : outputPath;

    try {
      D8Output d8InternalData =
          D8Utils.runD8Command(
              new D8Utils.D8DiagnosticsHandler(),
              d8Output,
              filesToDex,
              getD8Options(),
              primaryDexClassNamesPath,
              Paths.get(androidJar),
              classpathFiles,
              minSdkVersion);

      Collection<String> referencedResources = d8InternalData.getResources();
      Collection<String> referencedSynthetic = d8InternalData.getSynthetic();

      if (d8Output.endsWith(DEX_JAR_SUFFIX)) {
        ZipScrubber.scrubZip(d8Output);
      }

      Preconditions.checkState(
          primaryDexClassNamesPath.isPresent() || secondaryDexCompression == null);
      if (primaryDexClassNamesPath.isPresent()) {
        Path classesDotDex = d8Output.resolve("classes.dex");
        Preconditions.checkState(
            classesDotDex.toFile().exists(), "D8 command must produce a classes.dex");

        if ("jar".equals(secondaryDexCompression)) {
          Preconditions.checkState(outputDex.endsWith(DEX_JAR_SUFFIX));
          Preconditions.checkNotNull(secondaryDexMetadataFile);
          Path secondaryDexMetadataFilePath = Paths.get(secondaryDexMetadataFile);
          Preconditions.checkState(
              outputPath
                  .resolveSibling(outputPath.getFileName() + ".meta")
                  .toString()
                  .equals(secondaryDexMetadataFile));
          D8Utils.writeSecondaryDexJarAndMetadataFile(
              outputPath, secondaryDexMetadataFilePath, classesDotDex, "jar");
        } else {
          Preconditions.checkState(
              outputDex.endsWith(".dex"),
              String.format(
                  "Expect the outputDex to end with '.dex' if not '%s', but it is %s",
                  DEX_JAR_SUFFIX, outputDex));
          Files.move(classesDotDex, outputPath);
        }

        if ("jar".equals(secondaryDexCompression) || "raw".equals(secondaryDexCompression)) {
          Preconditions.checkNotNull(secondaryDexMetadataLine);
          Preconditions.checkNotNull(secondaryDexCanaryClassName);
          Files.write(
              Paths.get(secondaryDexMetadataLine),
              Collections.singletonList(
                  D8Utils.getSecondaryDexMetadataString(outputPath, secondaryDexCanaryClassName)));
        }
      }

      if (referencedResourcesPath.isPresent()) {
        List<String> sortedReferencedResources = new ArrayList<>(referencedResources);
        Collections.sort(sortedReferencedResources);
        Files.write(referencedResourcesPath.get(), sortedReferencedResources);
      }

      if (weightEstimatePath.isPresent() || classNamesPath.isPresent()) {
        int totalWeightEstimate = 0;
        ImmutableSet.Builder<String> classNames = ImmutableSet.builder();

        try (ZipFile zipFile = new ZipFile(Iterables.getOnlyElement(filesToDex).toFile())) {
          Enumeration<? extends ZipEntry> entries = zipFile.entries();
          while (entries.hasMoreElements()) {
            ZipEntry zipEntry = entries.nextElement();
            String zipEntryName = zipEntry.getName();
            // Ignore non-.class files.
            if (!zipEntryName.endsWith(CLASS_NAME_SUFFIX)) {
              continue;
            }

            classNames.add(
                zipEntryName.substring(0, zipEntryName.length() - CLASS_NAME_SUFFIX.length()));
            totalWeightEstimate += (int) zipEntry.getSize();
          }
        }

        ImmutableList<String> existingClassNames = ImmutableList.copyOf(classNames.build());
        // Add Synthetic classes generated by D8, if there is any, into classNames.
        if (referencedSynthetic != null) {
          for (String s : referencedSynthetic) {
            // The synthetic class exposed by D8 is in this format: "L...;", need to remove "L" and
            // ";"
            String syntheticName = s.substring(1, s.length() - 1);
            // TODO(T153432990) computeSynthesizedTypes() returns all synthesized types, including
            // those created from the library .jars. We only want the ones that were created from
            // the .jar that is being dexed.
            for (String existingClassName : existingClassNames) {
              if (syntheticName.startsWith(existingClassName)) {
                classNames.add(syntheticName);
                break;
              }
            }
          }
        }

        if (weightEstimatePath.isPresent()) {
          Preconditions.checkState(
              weightFactor != null,
              "Must specify `--weight-factor` if `--weight-estimate-path` is specified!");
          Files.write(
              weightEstimatePath.get(),
              Collections.singletonList(
                  Integer.toString(Integer.parseInt(weightFactor) * totalWeightEstimate)));
        }
        if (classNamesPath.isPresent()) {
          Files.write(classNamesPath.get(), classNames.build());
        }
      }
    } catch (CompilationFailedException e) {
      throw new IOException(e);
    }

    System.exit(0);
  }

  private Set<D8Options> getD8Options() {
    ImmutableSet.Builder<D8Options> d8OptionsBuilder = ImmutableSet.builder();
    if (intermediate) {
      d8OptionsBuilder.add(D8Options.INTERMEDIATE);
    }
    if (noOptimize) {
      d8OptionsBuilder.add(D8Options.NO_OPTIMIZE);
    }
    if (forceJumbo) {
      d8OptionsBuilder.add(D8Options.FORCE_JUMBO);
    }
    if (noDesugar) {
      d8OptionsBuilder.add(D8Options.NO_DESUGAR);
    }
    if (primaryDexClassNamesList != null) {
      // Only add the specified classes
      d8OptionsBuilder.add(D8Options.MINIMIZE_PRIMARY_DEX);
    }

    return d8OptionsBuilder.build();
  }
}
