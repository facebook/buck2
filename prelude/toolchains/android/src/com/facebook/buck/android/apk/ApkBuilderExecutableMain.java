/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.apk;

import com.android.apksig.ApkSigner;
import com.facebook.buck.android.apk.sdk.ApkCreationException;
import com.facebook.buck.android.apk.sdk.DuplicateFileException;
import com.facebook.buck.android.apk.sdk.SealedApkException;
import com.facebook.buck.android.zipalign.ZipAlign;
import com.facebook.buck.util.zip.RepackZipEntries;
import com.facebook.buck.util.zip.ZipCompressionLevel;
import com.facebook.infer.annotation.Nullsafe;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import org.jetbrains.annotations.Nullable;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/** Main entry point for executing {@link ApkBuilderUtils} calls. */
@Nullsafe(Nullsafe.Mode.LOCAL)
public class ApkBuilderExecutableMain {
  @Option(name = "--output-apk", required = true)
  // NULLSAFE_FIXME[Field Not Initialized]
  private String outputApk;

  @Option(name = "--resource-apk", required = true)
  // NULLSAFE_FIXME[Field Not Initialized]
  private String resourceApk;

  @Option(name = "--dex-file", required = true)
  // NULLSAFE_FIXME[Field Not Initialized]
  private String dexFile;

  @Option(name = "--keystore-path", required = true)
  // NULLSAFE_FIXME[Field Not Initialized]
  private String keystore;

  @Option(name = "--keystore-properties-path", required = true)
  // NULLSAFE_FIXME[Field Not Initialized]
  private String keystoreProperties;

  @Option(name = "--asset-directories-list", required = true)
  // NULLSAFE_FIXME[Field Not Initialized]
  private String assetDirectoriesList;

  @Option(name = "--native-libraries-directories-list", required = true)
  // NULLSAFE_FIXME[Field Not Initialized]
  private String nativeLibrariesDirectoriesList;

  @Option(name = "--zip-files-list", required = true)
  // NULLSAFE_FIXME[Field Not Initialized]
  private String zipFilesList;

  @Option(name = "--jar-files-that-may-contain-resources-list", required = true)
  // NULLSAFE_FIXME[Field Not Initialized]
  private String jarFilesThatMayContainResourcesList;

  @Option(name = "--zipalign_tool", required = true)
  // NULLSAFE_FIXME[Field Not Initialized]
  private String zipalignTool;

  @Option(name = "--compress-resources-dot-arsc")
  private boolean compressResourcesDotArsc;

  @Option(name = "--package-meta-inf-version-files")
  private boolean packageMetaInfVersionFiles;

  @Option(name = "--excluded-resources")
  @Nullable
  private String excludedResourcesList = null;

  @Option(name = "--uncompressed-files")
  @Nullable
  private String uncompressedFilesList = null;

  public static void main(String[] args) throws IOException {
    ApkBuilderExecutableMain main = new ApkBuilderExecutableMain();
    CmdLineParser parser = new CmdLineParser(main);
    try {
      parser.parseArgument(args);
      main.run();
      System.exit(0);
    } catch (CmdLineException e) {
      System.err.println(e.toString());
      parser.printUsage(System.err);
      System.exit(1);
    }
  }

  private void run() throws IOException {
    ImmutableSet<Path> assetDirectories =
        Files.readAllLines(Paths.get(assetDirectoriesList)).stream()
            .map(Paths::get)
            .collect(ImmutableSet.toImmutableSet());

    ImmutableSet<Path> nativeLibraryDirectories =
        Files.readAllLines(Paths.get(nativeLibrariesDirectoriesList)).stream()
            .map(Paths::get)
            .collect(ImmutableSet.toImmutableSet());

    ImmutableSet<Path> zipFiles =
        Files.readAllLines(Paths.get(zipFilesList)).stream()
            .map(Paths::get)
            .collect(ImmutableSet.toImmutableSet());

    ImmutableSet<Path> jarFilesThatMayContainResources =
        Files.readAllLines(Paths.get(jarFilesThatMayContainResourcesList)).stream()
            .map(Paths::get)
            .collect(ImmutableSet.toImmutableSet());

    ImmutableSet<String> excludedResources =
        excludedResourcesList == null
            ? ImmutableSet.of()
            : ImmutableSet.copyOf(Files.readAllLines(Paths.get(excludedResourcesList)));

    ImmutableSet<String> uncompressedFiles =
        uncompressedFilesList == null
            ? ImmutableSet.of()
            : ImmutableSet.copyOf(Files.readAllLines(Paths.get(uncompressedFilesList)));

    Path keystorePath = Paths.get(keystore);
    Path keystorePropertiesPath = Paths.get(keystoreProperties);

    Path intermediateApk = Files.createTempFile("intermediate", "output.apk");
    Path zipalignApk = Files.createTempFile("zipalign", "output.apk");
    KeystoreProperties keystoreProperties =
        KeystoreProperties.createFromPropertiesFile(keystorePath, keystorePropertiesPath);

    try {
      ApkBuilderUtils.buildApk(
          Paths.get(resourceApk),
          intermediateApk,
          Paths.get(dexFile),
          assetDirectories,
          nativeLibraryDirectories,
          zipFiles,
          jarFilesThatMayContainResources,
          keystorePath,
          keystoreProperties,
          packageMetaInfVersionFiles,
          null,
          excludedResources);

      if (compressResourcesDotArsc) {
        Path intermediateApkWithCompressedResources =
            Files.createTempFile("intermediate", "output_with_compressed_resources.apk");
        RepackZipEntries.repack(
            intermediateApk,
            intermediateApkWithCompressedResources,
            ImmutableSet.of("resources.arsc"),
            ZipCompressionLevel.MAX);
        intermediateApk = intermediateApkWithCompressedResources;
      }

      if (!uncompressedFiles.isEmpty()) {
        ImmutableSet<String> expandedUncompressedFiles =
            expandRegexPatterns(intermediateApk, uncompressedFiles);
        if (!expandedUncompressedFiles.isEmpty()) {
          Path intermediateApkWithUncompressedFiles =
              Files.createTempFile("intermediate", "output_with_uncompressed_files.apk");
          RepackZipEntries.repack(
              intermediateApk,
              intermediateApkWithUncompressedFiles,
              expandedUncompressedFiles,
              ZipCompressionLevel.NONE);
          intermediateApk = intermediateApkWithUncompressedFiles;
        }
      }

      ZipAlign zipAlign =
          new ZipAlign(zipalignTool, intermediateApk.toString(), zipalignApk.toString());
      zipAlign.run();

      ImmutableList<ApkSigner.SignerConfig> signerConfigs =
          ApkSignerUtils.getSignerConfigs(keystoreProperties, Files.newInputStream(keystorePath));
      ApkSignerUtils.signApkFile(
          zipalignApk.toFile(), Paths.get(outputApk).toFile(), signerConfigs);
    } catch (UnrecoverableKeyException
        | NoSuchAlgorithmException
        | ApkCreationException
        | SealedApkException
        | KeyStoreException
        | InterruptedException e) {
      throw new RuntimeException(e);
    } catch (DuplicateFileException e) {
      throw new RuntimeException(
          String.format(
              "Found duplicate file for APK: %1$s\nOrigin 1: %2$s\nOrigin 2: %3$s",
              e.getArchivePath(), e.getFile1(), e.getFile2()));
    }

    System.exit(0);
  }

  /**
   * Expands regex patterns in the given set of patterns to actual file names found in the APK.
   * Patterns are treated as regex patterns and matched against APK entries.
   *
   * @param apkPath Path to the APK file to scan for matching entries
   * @param patterns Set of regex patterns to match against APK entries
   * @return Set of actual file names that match the patterns
   */
  private static ImmutableSet<String> expandRegexPatterns(
      Path apkPath, ImmutableSet<String> patterns) throws IOException {
    Set<Pattern> regexPatterns = new HashSet<>();

    // Compile all patterns as regex
    for (String pattern : patterns) {
      regexPatterns.add(Pattern.compile(pattern));
    }

    // If no patterns, return empty set
    if (regexPatterns.isEmpty()) {
      return ImmutableSet.of();
    }

    // Scan the APK to find entries matching regex patterns
    Set<String> result = new HashSet<>();
    try (BufferedInputStream bufferedIn = new BufferedInputStream(Files.newInputStream(apkPath));
        ZipInputStream zipIn = new ZipInputStream(bufferedIn)) {
      ZipEntry entry;
      while ((entry = zipIn.getNextEntry()) != null) {
        String entryName = entry.getName();
        for (Pattern regexPattern : regexPatterns) {
          if (regexPattern.matcher(entryName).matches()) {
            result.add(entryName);
            break;
          }
        }
      }
    }

    return ImmutableSet.copyOf(result);
  }
}
