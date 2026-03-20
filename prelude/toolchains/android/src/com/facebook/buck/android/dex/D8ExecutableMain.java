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
import com.facebook.infer.annotation.Nullsafe;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
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
import org.jetbrains.annotations.Nullable;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/** Main entry point for executing {@link com.android.tools.r8.D8Command} calls. */
@Nullsafe(Nullsafe.Mode.LOCAL)
public class D8ExecutableMain {
  /** name suffix that identifies it as a Java class file. */
  private static final String CLASS_NAME_SUFFIX = ".class";

  @Option(name = "--output-dex-file", required = true)
  // NULLSAFE_FIXME[Field Not Initialized]
  private String outputDex;

  @Option(name = "--files-to-dex-list")
  @Nullable
  private String filesToDexList = null;

  @Option(name = "--file-to-dex")
  @Nullable
  private String fileToDex = null;

  @Option(name = "--android-jar", required = true)
  // NULLSAFE_FIXME[Field Not Initialized]
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
  @Nullable
  private String primaryDexClassNamesList = null;

  @Option(name = "--referenced-resources-path")
  @Nullable
  private String referencedResourcesList = null;

  @Option(name = "--classpath-files")
  @Nullable
  private String classpathFilesList = null;

  @Option(name = "--min-sdk-version")
  @Nullable
  private String minSdkVersionString = null;

  @Option(name = "--weight-estimate-path")
  @Nullable
  private String weightEstimateOutput = null;

  @Option(name = "--weight-factor")
  @Nullable
  private String weightFactor = null;

  @Option(name = "--class-names-path")
  @Nullable
  private String classNamesOutput = null;

  @Option(name = "--ref-count-path")
  @Nullable
  private String refCountOutput = null;

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
  @Nullable
  private String secondaryDexCompression = null;

  @Option(name = "--secondary-dex-metadata-file")
  @Nullable
  private String secondaryDexMetadataFile = null;

  @Option(name = "--secondary-dex-metadata-line")
  @Nullable
  private String secondaryDexMetadataLine = null;

  @Option(name = "--secondary-dex-canary-class-name")
  @Nullable
  private String secondaryDexCanaryClassName = null;

  private static final String DEX_JAR_SUFFIX = ".dex.jar";

  // DEX file header offsets for method and field ID counts.
  //
  // Reference: https://source.android.com/docs/core/runtime/dex-format#header-item
  //
  // The header_item fields are laid out sequentially with no padding.
  // Each field is a uint (4 bytes) unless noted otherwise:
  //
  //   Offset  Size  Field
  //   ------  ----  -----
  //        0     8  magic (ubyte[8])
  //        8     4  checksum
  //       12    20  signature (ubyte[20])
  //       32     4  file_size
  //       36     4  header_size
  //       40     4  endian_tag
  //       44     4  link_size
  //       48     4  link_off
  //       52     4  map_off
  //       56     4  string_ids_size
  //       60     4  string_ids_off
  //       64     4  type_ids_size     <-- DEX_TYPE_IDS_SIZE_OFFSET
  //       68     4  type_ids_off
  //       72     4  proto_ids_size
  //       76     4  proto_ids_off
  //       80     4  field_ids_size    <-- DEX_FIELD_IDS_SIZE_OFFSET
  //       84     4  field_ids_off
  //       88     4  method_ids_size   <-- DEX_METHOD_IDS_SIZE_OFFSET
  //       92     4  method_ids_off
  //       96     4  class_defs_size
  //      100     4  class_defs_off
  //      104     4  data_size
  //      108     4  data_off
  //      ---  v40 header ends here (112 bytes = 0x70)  ---
  //      112     4  container_size    (v41+ only)
  //      116     4  header_offset     (v41+ only)
  //      ---  v41 header ends here (120 bytes = 0x78)  ---
  //
  // field_ids_size and method_ids_size are at offsets 80 and 88 in both
  // versions. We read 120 bytes to cover the larger v41+ header. For v40
  // files the extra 8 bytes come from the data section, which is harmless
  // since we only inspect offsets 80 and 88.
  private static final int DEX_HEADER_SIZE = 120;
  private static final int DEX_TYPE_IDS_SIZE_OFFSET = 64;
  private static final int DEX_FIELD_IDS_SIZE_OFFSET = 80;
  private static final int DEX_METHOD_IDS_SIZE_OFFSET = 88;

  public static void main(String[] args) throws IOException {
    D8ExecutableMain main = new D8ExecutableMain();
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

      // Read method and field reference counts from the output DEX file header.
      // The DEX format has hard limits of 64K method_ids and 64K field_ids per file.
      // Unlike the weight_estimate (which uses uncompressed .class byte sizes as a proxy),
      // these counts are exact and can be used for precise secondary DEX splitting.
      if (refCountOutput != null) {
        Path refCountPath = Paths.get(refCountOutput);
        int methodRefCount = 0;
        int fieldRefCount = 0;
        int typeRefCount = 0;

        // For pre-dex (intermediate) mode, the output is a .dex.jar containing one or
        // more .dex files (classes.dex, classes2.dex, ...).  We sum ref counts across
        // all .dex entries so that the conservative upper bound covers multi-dex jars.
        // For merge mode, the output is a bare .dex file.
        // Some libraries produce empty .dex.jar files (no Java classes, e.g. resource-only
        // AARs). In that case, writing "0 0 0" is correct -- the library contributes no
        // method/field/type refs to any dex file.
        if (outputPath.toString().endsWith(DEX_JAR_SUFFIX)) {
          try (ZipFile dexJar = new ZipFile(outputPath.toFile())) {
            DexRefCounts jarCounts = readDexRefCountsFromJar(dexJar);
            methodRefCount = jarCounts.methodIds;
            fieldRefCount = jarCounts.fieldIds;
            typeRefCount = jarCounts.typeIds;
          }
        } else if (outputPath.toString().endsWith(".dex")) {
          if (!outputPath.toFile().exists()) {
            throw new IOException(
                "--ref-count-path requested but output " + outputPath + " does not exist");
          }
          try (InputStream is = Files.newInputStream(outputPath)) {
            DexRefCounts counts = readDexRefCounts(is);
            methodRefCount = counts.methodIds;
            fieldRefCount = counts.fieldIds;
            typeRefCount = counts.typeIds;
          }
        } else {
          throw new IOException(
              "--ref-count-path requested but output "
                  + outputPath
                  + " is neither a .dex.jar nor a .dex file");
        }

        Files.write(
            refCountPath,
            Collections.singletonList(methodRefCount + " " + fieldRefCount + " " + typeRefCount));
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

  /** Exact ref counts from a DEX file header (method_ids_size, field_ids_size, type_ids_size). */
  static class DexRefCounts {
    final int methodIds;
    final int fieldIds;
    final int typeIds;

    DexRefCounts(int methodIds, int fieldIds, int typeIds) {
      this.methodIds = methodIds;
      this.fieldIds = fieldIds;
      this.typeIds = typeIds;
    }
  }

  /** Reads the method_ids_size, field_ids_size, and type_ids_size from a DEX file header. */
  static DexRefCounts readDexRefCounts(InputStream is) throws IOException {
    byte[] header = new byte[DEX_HEADER_SIZE];
    int bytesRead = 0;
    while (bytesRead < DEX_HEADER_SIZE) {
      int n = is.read(header, bytesRead, DEX_HEADER_SIZE - bytesRead);
      if (n < 0) {
        throw new IOException(
            "Unexpected end of DEX file: read only "
                + bytesRead
                + " of "
                + DEX_HEADER_SIZE
                + " header bytes");
      }
      bytesRead += n;
    }

    ByteBuffer buf = ByteBuffer.wrap(header).order(ByteOrder.LITTLE_ENDIAN);
    return new DexRefCounts(
        buf.getInt(DEX_METHOD_IDS_SIZE_OFFSET),
        buf.getInt(DEX_FIELD_IDS_SIZE_OFFSET),
        buf.getInt(DEX_TYPE_IDS_SIZE_OFFSET));
  }

  /** Reads and sums ref counts across all .dex entries in a .dex.jar (handles multi-dex). */
  static DexRefCounts readDexRefCountsFromJar(ZipFile dexJar) throws IOException {
    int methodRefCount = 0;
    int fieldRefCount = 0;
    int typeRefCount = 0;
    Enumeration<? extends ZipEntry> entries = dexJar.entries();
    while (entries.hasMoreElements()) {
      ZipEntry entry = entries.nextElement();
      if (entry.getName().endsWith(".dex")) {
        try (InputStream is = dexJar.getInputStream(entry)) {
          DexRefCounts counts = readDexRefCounts(is);
          methodRefCount += counts.methodIds;
          fieldRefCount += counts.fieldIds;
          typeRefCount += counts.typeIds;
        }
      }
    }
    return new DexRefCounts(methodRefCount, fieldRefCount, typeRefCount);
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
