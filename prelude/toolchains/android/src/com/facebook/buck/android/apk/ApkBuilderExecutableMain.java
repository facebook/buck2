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
import com.facebook.buck.core.exceptions.HumanReadableException;
import com.facebook.buck.util.zip.RepackZipEntries;
import com.facebook.buck.util.zip.ZipCompressionLevel;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.io.CharStreams;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/** Main entry point for executing {@link ApkBuilderUtils} calls. */
public class ApkBuilderExecutableMain {
  @Option(name = "--output-apk", required = true)
  private String outputApk;

  @Option(name = "--resource-apk", required = true)
  private String resourceApk;

  @Option(name = "--dex-file", required = true)
  private String dexFile;

  @Option(name = "--keystore-path", required = true)
  private String keystore;

  @Option(name = "--keystore-properties-path", required = true)
  private String keystoreProperties;

  @Option(name = "--asset-directories-list", required = true)
  private String assetDirectoriesList;

  @Option(name = "--native-libraries-directories-list", required = true)
  private String nativeLibrariesDirectoriesList;

  @Option(name = "--zip-files-list", required = true)
  private String zipFilesList;

  @Option(name = "--jar-files-that-may-contain-resources-list", required = true)
  private String jarFilesThatMayContainResourcesList;

  @Option(name = "--zipalign_tool", required = true)
  private String zipalignTool;

  @Option(name = "--compress-resources-dot-arsc")
  private boolean compressResourcesDotArsc;

  @Option(name = "--package-meta-inf-version-files")
  private boolean packageMetaInfVersionFiles;

  @Option(name = "--excluded-resources")
  private String excludedResourcesList;

  public static void main(String[] args) throws IOException {
    ApkBuilderExecutableMain main = new ApkBuilderExecutableMain();
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
      Process zipalignProcess =
          new ProcessBuilder()
              .command(zipalignTool, "-f", "4", intermediateApk.toString(), zipalignApk.toString())
              .start();
      zipalignProcess.waitFor();
      if (zipalignProcess.exitValue() != 0) {
        String errorMessage = null;
        try (Reader reader = new InputStreamReader(zipalignProcess.getErrorStream())) {
          errorMessage = CharStreams.toString(reader);
        }
        if (errorMessage.contains("Unable to open")) {
          errorMessage =
              errorMessage.concat(
                  "\n"
                      + "This issue is usually caused by having more than 2^^16 files in the APK."
                      + " Try filtering out some resources.\n");
        }

        throw new RuntimeException("zipalign failed to process apk file:\n" + errorMessage);
      }

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
      throw new HumanReadableException(
          String.format(
              "Found duplicate file for APK: %1$s\nOrigin 1: %2$s\nOrigin 2: %3$s",
              e.getArchivePath(), e.getFile1(), e.getFile2()));
    }

    System.exit(0);
  }
}
