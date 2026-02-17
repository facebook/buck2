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
import com.android.tools.r8.CompilationMode;
import com.android.tools.r8.D8Command;
import com.android.tools.r8.Diagnostic;
import com.android.tools.r8.DiagnosticsHandler;
import com.android.tools.r8.OutputMode;
import com.android.tools.r8.utils.InternalOptions;
import com.facebook.buck.android.apkmodule.APKModule;
import com.facebook.buck.util.zip.CustomZipOutputStream;
import com.facebook.buck.util.zip.ZipOutputStreams;
import com.facebook.buck.util.zip.ZipScrubber;
import com.facebook.infer.annotation.Nullsafe;
import com.google.common.base.Preconditions;
import com.google.common.hash.Hashing;
import com.google.common.io.ByteStreams;
import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/** Runs d8. */
@Nullsafe(Nullsafe.Mode.LOCAL)
public class D8Utils {

  public static D8Output runD8Command(
      D8DiagnosticsHandler diagnosticsHandler,
      Path outputDexFile,
      Iterable<Path> filesToDex,
      Set<D8Options> options,
      Optional<Path> primaryDexClassNamesPath,
      Path androidJarPath,
      Collection<Path> classpathFiles,
      Optional<Integer> minSdkVersion)
      throws CompilationFailedException, IOException {
    Set<Path> inputs = new HashSet<>();
    for (Path toDex : filesToDex) {
      if (Files.isRegularFile(toDex)) {
        inputs.add(toDex);
      } else {
        try (Stream<Path> paths = Files.walk(toDex)) {
          paths.filter(path -> path.toFile().isFile()).forEach(inputs::add);
        }
      }
    }

    // D8 only outputs to dex if the output path is a directory. So we output to a temporary dir
    // and move it over to the final location
    boolean outputToDex = outputDexFile.getFileName().toString().endsWith(".dex");
    Path output = outputToDex ? Files.createTempDirectory("buck-d8") : outputDexFile;

    // NULLSAFE_FIXME[Unvetted Third Party In Nullsafe]
    D8Command.Builder builder =
        // NULLSAFE_FIXME[Unvetted Third Party In Nullsafe]
        D8Command.builder(diagnosticsHandler)
            // NULLSAFE_FIXME[Unvetted Third Party In Nullsafe]
            .addProgramFiles(inputs)
            // NULLSAFE_FIXME[Unvetted Third Party In Nullsafe]
            .setIntermediate(options.contains(D8Options.INTERMEDIATE))
            // NULLSAFE_FIXME[Unvetted Third Party In Nullsafe]
            .addLibraryFiles(androidJarPath)
            .setMode(
                options.contains(D8Options.NO_OPTIMIZE)
                    ? CompilationMode.DEBUG
                    : CompilationMode.RELEASE)
            // NULLSAFE_FIXME[Unvetted Third Party In Nullsafe]
            .setOutput(output, OutputMode.DexIndexed)
            // NULLSAFE_FIXME[Unvetted Third Party In Nullsafe]
            .setDisableDesugaring(options.contains(D8Options.NO_DESUGAR))
            // NULLSAFE_FIXME[Unvetted Third Party In Nullsafe]
            .setInternalOptionsModifier(
                (InternalOptions opt) -> {
                  // NULLSAFE_FIXME[Unvetted Third Party In Nullsafe]
                  opt.testing.forceJumboStringProcessing = options.contains(D8Options.FORCE_JUMBO);
                  if (options.contains(D8Options.MINIMIZE_PRIMARY_DEX)) {
                    opt.minimalMainDex = true;
                  } else if (options.contains(D8Options.MAXIMIZE_PRIMARY_DEX)) {
                    opt.minimalMainDex = false;
                  }
                });

    minSdkVersion.ifPresent(builder::setMinApiLevel);
    if (minSdkVersion.orElse(0) <= 21) {
      // addMainDexListFiles is not supported for minSdkVersion > 21
      primaryDexClassNamesPath.ifPresent(builder::addMainDexListFiles);
    }

    if (classpathFiles != null) {
      // classpathFiles is needed only for D8 Java 8 desugar
      builder.addClasspathFiles(classpathFiles);
    }

    // NULLSAFE_FIXME[Unvetted Third Party In Nullsafe]
    D8Command d8Command = builder.build();
    com.android.tools.r8.D8.run(d8Command);

    if (outputToDex) {
      File[] outputs = output.toFile().listFiles();
      if (outputs != null && (outputs.length > 0)) {
        Files.move(outputs[0].toPath(), outputDexFile, StandardCopyOption.REPLACE_EXISTING);
      }
    }

    // NULLSAFE_FIXME[Unvetted Third Party In Nullsafe]
    return new D8Output(
        d8Command.getDexItemFactory().computeReferencedResources(),
        d8Command.getDexItemFactory().computeSynthesizedTypes());
  }

  static void writeSecondaryDexJarAndMetadataFile(
      Path secondaryDexOutputJarPath,
      Path secondaryDexOutputJarMetadataPath,
      Path rawSecondaryDexPath,
      String compression)
      throws IOException {

    try (CustomZipOutputStream jarOutputStream =
            ZipOutputStreams.newOutputStream(secondaryDexOutputJarPath);
        InputStream secondaryDexInputStream =
            new BufferedInputStream(new FileInputStream(rawSecondaryDexPath.toFile()))) {

      ZipEntry customEntry = new ZipEntry("classes.dex");
      if (compression.equals("xz") || compression.equals("xzs")) {
        try (ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
          ByteStreams.copy(secondaryDexInputStream, bos);
          byte[] bytes = bos.toByteArray();
          customEntry.setCrc(Hashing.crc32().hashBytes(bytes).padToLong());
          customEntry.setSize(bytes.length);
          customEntry.setCompressedSize(bytes.length);
          customEntry.setMethod(ZipEntry.STORED);
          jarOutputStream.putNextEntry(customEntry);
          ByteStreams.copy(new ByteArrayInputStream(bytes), jarOutputStream);
          jarOutputStream.closeEntry();
        }
      } else {
        jarOutputStream.putNextEntry(customEntry);
        ByteStreams.copy(secondaryDexInputStream, jarOutputStream);
        jarOutputStream.closeEntry();
      }
    }

    ZipScrubber.scrubZip(secondaryDexOutputJarPath);

    writeSecondaryDexMetadata(
        secondaryDexOutputJarPath, secondaryDexOutputJarMetadataPath, compression);
  }

  /**
   * Write a secondary dex jar metadata file. This is a .meta file with a single line containing:
   *
   * <p>jar:<size of secondary dex jar (in bytes)> dex:<size of uncompressed dex file (in bytes)>
   */
  static void writeSecondaryDexMetadata(
      Path secondaryDexOutputJarPath, Path secondaryDexOutputJarMetadataPath, String compression)
      throws IOException {
    try (ZipFile zf = new ZipFile(secondaryDexOutputJarPath.toFile())) {
      ZipEntry classesDexEntry = zf.getEntry("classes.dex");
      if (classesDexEntry == null) {
        throw new RuntimeException("could not find classes.dex in jar");
      }

      long uncompressedSize = classesDexEntry.getSize();
      if (uncompressedSize == -1) {
        throw new RuntimeException("classes.dex size should be known");
      }

      long jarSize = Files.size(secondaryDexOutputJarPath);
      if (compression.equals("xz") || compression.equals("xzs")) {
        Preconditions.checkState(
            uncompressedSize + 120 == jarSize,
            "For xz and xzs compression, we expect the .dex to be stored uncompressed and the "
                + "overhead of the .jar itself to be 120 bytes!");
      }

      Files.write(
          secondaryDexOutputJarMetadataPath,
          Collections.singletonList(
              String.format(
                  "jar:%s dex:%s", Files.size(secondaryDexOutputJarPath), uncompressedSize)));
    }
  }

  /**
   * The secondary dex directory contains a single metadata.txt file which has one line per
   * secondary dex, consisting of:
   *
   * <p><secondary dex file name> <sha1 hash of secondary dex> <canary class>
   */
  static String getSecondaryDexMetadataString(Path secondaryDexPath, String canaryClassName)
      throws IOException {
    return String.format(
        "%s %s %s",
        secondaryDexPath.getFileName(),
        com.google.common.io.Files.hash(secondaryDexPath.toFile(), Hashing.sha1()).toString(),
        canaryClassName);
  }

  static String getRawSecondaryDexSubDir(String module) {
    if (APKModule.isRootModule(module)) {
      return "";
    } else {
      return String.format("assets/%s", module);
    }
  }

  public static class D8DiagnosticsHandler implements DiagnosticsHandler {

    public final List<Diagnostic> diagnostics = new ArrayList<>();

    @Override
    public void warning(Diagnostic warning) {
      diagnostics.add(warning);
    }

    @Override
    public void info(Diagnostic info) {}
  }
}
