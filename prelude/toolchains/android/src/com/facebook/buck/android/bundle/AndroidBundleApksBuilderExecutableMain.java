/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.bundle;

import com.android.apksig.ApkSigner;
import com.android.tools.build.bundletool.androidtools.Aapt2Command;
import com.android.tools.build.bundletool.androidtools.P7ZipCommand;
import com.android.tools.build.bundletool.commands.BuildApksCommand;
import com.facebook.buck.android.apk.ApkSignerUtils;
import com.facebook.buck.android.apk.KeystoreProperties;
import com.facebook.buck.util.zip.ZipScrubber;
import com.google.common.collect.ImmutableList;
import com.google.common.io.CharStreams;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

public class AndroidBundleApksBuilderExecutableMain {

  @Option(name = "--output-apk", required = true)
  private Path outputApk;

  @Option(name = "--input-bundle", required = true)
  private Path inputBundle;

  @Option(name = "--p7zip", required = true)
  private Path p7zipBinaryPath;

  @Option(name = "--aapt2", required = true)
  private Path aapt2BinaryPath;

  @Option(name = "--zipalign", required = true)
  private String zipalignTool;

  @Option(name = "--keystore", depends = "--keystore-properties")
  private Path keystorePath;

  @Option(name = "--keystore-properties", depends = "--keystore")
  private Path keystorePropertiesPath;

  private void run() throws Exception {
    Path apksDirectory = Files.createTempDirectory("derived.apks");
    BuildApksCommand.Builder buildApksCommandBuilder =
        BuildApksCommand.builder()
            .setApkBuildMode(BuildApksCommand.ApkBuildMode.UNIVERSAL)
            .setAapt2Command(Aapt2Command.createFromExecutablePath(aapt2BinaryPath))
            .setP7ZipCommand(P7ZipCommand.defaultP7ZipCommand(p7zipBinaryPath, 4))
            .setOutputFormat(BuildApksCommand.OutputFormat.DIRECTORY)
            .setOutputFile(apksDirectory)
            .setBundlePath(inputBundle);

    buildApksCommandBuilder.build().execute();

    Path universalApkPath = apksDirectory.resolve("universal.apk");
    Path zipalignedApk = apksDirectory.resolve("universal-zipaligned.apk");

    ZipScrubber.scrubZip(universalApkPath);

    Process zipalignProcess =
        new ProcessBuilder()
            .command(zipalignTool, "-f", "4", universalApkPath.toString(), zipalignedApk.toString())
            .start();
    zipalignProcess.waitFor();
    if (zipalignProcess.exitValue() != 0) {
      try (Reader reader = new InputStreamReader(zipalignProcess.getErrorStream())) {
        String errorMessage = CharStreams.toString(reader);
        throw new RuntimeException("zipalign failed to process apk file:\n" + errorMessage);
      }
    }

    if (keystorePath != null && keystorePropertiesPath != null) {
      KeystoreProperties keystoreProperties =
          KeystoreProperties.createFromPropertiesFile(keystorePath, keystorePropertiesPath);

      ImmutableList<ApkSigner.SignerConfig> signerConfigs =
          ApkSignerUtils.getSignerConfigs(keystoreProperties, Files.newInputStream(keystorePath));
      ApkSignerUtils.signApkFile(zipalignedApk.toFile(), outputApk.toFile(), signerConfigs);
    } else {
      Files.copy(zipalignedApk, outputApk);
    }
  }

  public static void main(String[] args) throws IOException {
    AndroidBundleApksBuilderExecutableMain main = new AndroidBundleApksBuilderExecutableMain();
    CmdLineParser parser = new CmdLineParser(main);
    try {
      parser.parseArgument(args);
      main.run();
      System.exit(0);
    } catch (CmdLineException e) {
      System.err.println(e.getMessage());
      parser.printUsage(System.err);
      System.exit(1);
    } catch (Exception e) {
      System.err.println(e.getMessage());
      System.exit(1);
    }
  }
}
