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

import com.facebook.buck.android.apkmodule.APKModule;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.io.ByteStreams;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;
import org.tukaani.xz.LZMA2Options;
import org.tukaani.xz.XZ;
import org.tukaani.xz.XZOutputStream;

/** Executable for compressing secondary dex files. */
public class SecondaryDexCompressionExecutableMain {
  @Option(name = "--secondary-dex-output-dir", required = true)
  private String secondaryDexOutputDirString;

  @Option(name = "--raw-secondary-dexes-dir", required = true)
  private String rawSecondaryDexesDir;

  @Option(name = "--module", required = true)
  private String module;

  @Option(name = "--module-deps")
  private String moduleDepsPathString;

  @Option(name = "--canary-class-name", required = true)
  private String canaryClassName;

  @Option(name = "--compression", required = true)
  private String compression;

  @Option(name = "--xz-compression-level")
  private int xzCompressionLevel = -1;

  // Optional, if this is the main module there may be N dex files that are being treated as
  // preceding these given secondary dex files.
  @Option(name = "--bootstrap-dexes-dir")
  private String bootstrapDexDirString;

  // Defaulted to 1 for the primary dex (classes.dex) upon which these secondaries will be numbered
  // after. If enabling bootstrap dex files, secondaries could start at a higher index.
  private int baseApkDexFileCount;

  public static void main(String[] args) throws IOException {
    SecondaryDexCompressionExecutableMain main = new SecondaryDexCompressionExecutableMain();
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
    Path rawSecondaryDexesDirPath = Paths.get(rawSecondaryDexesDir);
    Preconditions.checkState(
        ImmutableList.of("raw", "jar", "xz", "xzs").contains(compression),
        "Only raw, jar, xz and xzs compression is supported!");
    Preconditions.checkState(
        compression.equals("raw") || compression.equals("jar") || xzCompressionLevel != -1,
        "Must specify a valid compression level when xz or xzs compression is used!");

    if (bootstrapDexDirString != null) {
      Path bootstrapDexDir = Paths.get(bootstrapDexDirString);
      baseApkDexFileCount = 1 + (int) Files.list(bootstrapDexDir).count();
    } else {
      baseApkDexFileCount = 1;
    }
    Path secondaryDexOutputDir = Paths.get(secondaryDexOutputDirString);
    Files.createDirectories(secondaryDexOutputDir);
    Path secondaryDexSubdir = secondaryDexOutputDir.resolve(getSecondaryDexSubDir(module));
    Files.createDirectories(secondaryDexSubdir);

    long secondaryDexCount = Files.list(rawSecondaryDexesDirPath).count();
    ImmutableList.Builder<String> metadataLines = ImmutableList.builder();
    if (!APKModule.isRootModule(module)) {
      metadataLines.add(String.format(".id %s", module));
    }
    Preconditions.checkState((moduleDepsPathString == null) == APKModule.isRootModule(module));
    if (moduleDepsPathString != null) {
      metadataLines.addAll(
          Files.readAllLines(Paths.get(moduleDepsPathString)).stream()
              .map(moduleDep -> String.format(".requires %s", moduleDep))
              .collect(ImmutableList.toImmutableList()));
    }

    if (compression.equals("raw")) {
      if (APKModule.isRootModule(module)) {
        metadataLines.add(".root_relative");
      }
      for (int i = 0; i < secondaryDexCount; i++) {
        String secondaryDexName = getRawSecondaryDexName(module, i);
        Path secondaryDexSubDir =
            secondaryDexOutputDir.resolve(D8Utils.getRawSecondaryDexSubDir(module));
        Path copiedDex = secondaryDexSubDir.resolve(secondaryDexName);
        Files.copy(rawSecondaryDexesDirPath.resolve(secondaryDexName), copiedDex);
        metadataLines.add(
            D8Utils.getSecondaryDexMetadataString(
                copiedDex, CanaryUtils.getFullyQualifiedCanaryClassName(canaryClassName, i)));
      }
    } else {
      ImmutableList.Builder<Path> secondaryDexJarPaths = ImmutableList.builder();
      for (int i = 0; i < secondaryDexCount; i++) {
        String secondaryDexName = getRawSecondaryDexName(module, i);
        Path rawSecondaryDexPath = rawSecondaryDexesDirPath.resolve(secondaryDexName);
        Preconditions.checkState(
            Files.exists(rawSecondaryDexPath), "Expected file to exist at: " + rawSecondaryDexPath);
        Path secondaryDexOutputJarPath =
            compression.equals("xzs")
                ? secondaryDexSubdir.resolve(
                    String.format("%s.xzs.tmp~", getSecondaryDexJarName(module, i)))
                : secondaryDexSubdir.resolve(getSecondaryDexJarName(module, i));
        secondaryDexJarPaths.add(secondaryDexOutputJarPath);

        Path metadataPath =
            secondaryDexOutputJarPath.resolveSibling(
                secondaryDexOutputJarPath.getFileName() + ".meta");
        D8Utils.writeSecondaryDexJarAndMetadataFile(
            secondaryDexOutputJarPath, metadataPath, rawSecondaryDexPath, compression);

        Path secondaryDexOutput;
        if (compression.equals("xz")) {
          secondaryDexOutput = doXzCompression(secondaryDexOutputJarPath);
        } else {
          secondaryDexOutput = secondaryDexOutputJarPath;
        }

        metadataLines.add(
            D8Utils.getSecondaryDexMetadataString(
                secondaryDexOutput, String.format("%s.dex%02d.Canary", canaryClassName, i + 1)));
      }

      if (compression.equals("xzs")) {
        doXzsCompression(secondaryDexSubdir, secondaryDexJarPaths.build());
      }
    }

    Files.write(secondaryDexSubdir.resolve("metadata.txt"), metadataLines.build());
  }

  private String getRawSecondaryDexName(String module, int index) {
    if (APKModule.isRootModule(module)) {
      return String.format("classes%d.dex", index + baseApkDexFileCount + 1);
    } else if (index == 0) {
      return "classes.dex";
    } else {
      return String.format("classes%d.dex", index + 1);
    }
  }

  private String getSecondaryDexSubDir(String module) {
    if (APKModule.isRootModule(module)) {
      return "assets/secondary-program-dex-jars";
    } else {
      return String.format("assets/%s", module);
    }
  }

  private String getSecondaryDexJarName(String module, int index) {
    return String.format(
        "%s-%d.dex.jar", APKModule.isRootModule(module) ? "secondary" : module, index + 1);
  }

  private Path doXzCompression(Path secondaryDexOutputJarPath) throws IOException {
    Path xzCompressedOutputJarPath =
        secondaryDexOutputJarPath.resolveSibling(secondaryDexOutputJarPath.getFileName() + ".xz");

    try (InputStream in =
            new BufferedInputStream(new FileInputStream(secondaryDexOutputJarPath.toFile()));
        OutputStream out =
            new BufferedOutputStream(new FileOutputStream(xzCompressedOutputJarPath.toFile()));
        XZOutputStream xzOut =
            new XZOutputStream(out, new LZMA2Options(xzCompressionLevel), XZ.CHECK_CRC32)) {
      ByteStreams.copy(in, xzOut);
    }

    Files.delete(secondaryDexOutputJarPath);

    return xzCompressedOutputJarPath;
  }

  private void doXzsCompression(Path secondaryDexSubdir, ImmutableList<Path> secondaryDexJarPaths)
      throws IOException {
    try (OutputStream secondaryDexOutput =
            new BufferedOutputStream(
                new FileOutputStream(
                    secondaryDexSubdir
                        .resolve(
                            String.format(
                                "%s.dex.jar.xzs",
                                APKModule.isRootModule(module) ? "secondary" : module))
                        .toFile()));
        XZOutputStream xzOutputStream =
            new XZOutputStream(
                secondaryDexOutput, new LZMA2Options(xzCompressionLevel), XZ.CHECK_CRC32)) {
      for (Path secondaryDexJarPath : secondaryDexJarPaths) {
        try (InputStream secondaryDexInputStream =
            new BufferedInputStream(new FileInputStream(secondaryDexJarPath.toFile()))) {
          ByteStreams.copy(secondaryDexInputStream, xzOutputStream);
        }
      }
    }
  }
}
