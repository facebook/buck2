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

import com.facebook.buck.android.proguard.ProguardTranslatorFactory;
import com.google.common.io.ByteStreams;
import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Enumeration;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.jar.JarOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/**
 * Main entry point for splitting a .jar into the .class files that should go into the primary .dex,
 * and the .class files that should go into a secondary .dex.
 */
public class JarSplitterExecutableMain {
  /** name suffix that identifies it as a Java class file. */
  private static final String CLASS_NAME_SUFFIX = ".class";

  @Option(name = "--input-jar", required = true)
  private String inputJar;

  @Option(name = "--primary-dex-patterns", required = true)
  private String primaryDexPatterns;

  @Option(name = "--proguard-configuration-file")
  private String proguardConfigurationFileString;

  @Option(name = "--proguard-mapping-file")
  private String proguardMappingFileString;

  @Option(name = "--primary-dex-classes-jar", required = true)
  private String primaryDexClassesJar;

  @Option(name = "--secondary-dex-classes-jar", required = true)
  private String secondaryDexClassesJar;

  public static void main(String[] args) throws IOException {
    JarSplitterExecutableMain main = new JarSplitterExecutableMain();
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
    ProguardTranslatorFactory proguardTranslatorFactory =
        ProguardTranslatorFactory.create(
            Optional.ofNullable(proguardConfigurationFileString).map(Paths::get),
            Optional.ofNullable(proguardMappingFileString).map(Paths::get),
            false);
    Function<String, String> deobfuscateFunction =
        proguardTranslatorFactory.createDeobfuscationFunction();
    ClassNameFilter primaryDexClassNameFilter =
        ClassNameFilter.fromConfiguration(Files.readAllLines(Paths.get(primaryDexPatterns)));

    try (JarOutputStream primaryJarOutputStream =
            new JarOutputStream(
                new BufferedOutputStream(
                    new FileOutputStream(Paths.get(primaryDexClassesJar).toFile())));
        JarOutputStream secondaryJarOutputStream =
            new JarOutputStream(
                new BufferedOutputStream(
                    new FileOutputStream(Paths.get(secondaryDexClassesJar).toFile())));
        ZipFile zipFile = new ZipFile(Paths.get(inputJar).toFile())) {
      Enumeration<? extends ZipEntry> entries = zipFile.entries();
      while (entries.hasMoreElements()) {
        ZipEntry zipEntry = entries.nextElement();
        String zipEntryName = zipEntry.getName();
        // Ignore non-.class files.
        if (!zipEntryName.endsWith(CLASS_NAME_SUFFIX)) {
          continue;
        }

        String deobfuscatedClassName =
            Objects.requireNonNull(
                deobfuscateFunction.apply(
                    zipEntryName.substring(0, zipEntryName.length() - CLASS_NAME_SUFFIX.length())));
        JarOutputStream jarOutputStream =
            primaryDexClassNameFilter.matches(deobfuscatedClassName)
                ? primaryJarOutputStream
                : secondaryJarOutputStream;
        zipEntry.setCompressedSize(-1);
        jarOutputStream.putNextEntry(zipEntry);
        ByteStreams.copy(zipFile.getInputStream(zipEntry), jarOutputStream);
        jarOutputStream.closeEntry();
      }
    }
  }
}
