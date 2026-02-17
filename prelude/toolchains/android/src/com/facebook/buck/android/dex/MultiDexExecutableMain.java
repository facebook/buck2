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
import com.facebook.buck.android.apkmodule.APKModule;
import com.facebook.buck.android.proguard.ProguardTranslatorFactory;
import com.facebook.infer.annotation.Nullsafe;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.EnumSet;
import java.util.Enumeration;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import org.jetbrains.annotations.Nullable;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/** Executable for creating multiple DEX files from library jars. */
@Nullsafe(Nullsafe.Mode.LOCAL)
public class MultiDexExecutableMain {
  /** name suffix that identifies it as a Java class file. */
  private static final String CLASS_NAME_SUFFIX = ".class";

  @Option(name = "--primary-dex")
  @Nullable
  private String primaryDexString = null;

  @Option(name = "--secondary-dex-output-dir", required = true)
  // NULLSAFE_FIXME[Field Not Initialized]
  private String secondaryDexOutputDirString;

  @Option(name = "--primary-dex-files-to-dex-list")
  @Nullable
  private String primaryDexFilesToDexList = null;

  @Option(name = "--files-to-dex-list")
  // NULLSAFE_FIXME[Field Not Initialized]
  private String filesToDexList;

  @Option(name = "--module", required = true)
  // NULLSAFE_FIXME[Field Not Initialized]
  private String module;

  @Option(name = "--canary-class-name", required = true)
  // NULLSAFE_FIXME[Field Not Initialized]
  private String canaryClassName;

  @Option(name = "--android-jar")
  // NULLSAFE_FIXME[Field Not Initialized]
  private String androidJar;

  @Option(name = "--primary-dex-patterns-path")
  @Nullable
  private String primaryDexPatternsPathString = null;

  @Option(name = "--primary-dex-class-names")
  @Nullable
  private String deobfuscatedPrimaryDexClassNamesPathString = null;

  @Option(name = "--proguard-configuration-file")
  @Nullable
  private String proguardConfigurationFileString = null;

  @Option(name = "--proguard-mapping-file")
  @Nullable
  private String proguardMappingFileString = null;

  @Option(name = "--min-sdk-version")
  @Nullable
  private String minSdkVersionString = null;

  @Option(name = "--no-optimize")
  private boolean noOptimize = false;

  @Option(name = "--minimize-primary-dex")
  private boolean minimizePrimaryDex = false;

  @Option(name = "--enable-bootstrap-dexes")
  private boolean enableBootstrapDexes = false;

  @Option(name = "--bootstrap-dex-output-dir")
  @Nullable
  private String bootstrapDexOutputDirString = null;

  @Option(name = "--classpath-files")
  @Nullable
  private Path classpathFilesList = null;

  public static void main(String[] args) throws IOException {
    MultiDexExecutableMain main = new MultiDexExecutableMain();
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

  // This handles a few scenarios:
  // 1) Root module, minimize primary dex
  //    a) Run d8 once to create the minimal primary dex with the given primary dex jars, and files
  //       to dex as part of the class path.
  //    b) Run d8 again to create N secondaries, creating and using a canary class as this run's
  //       primary dex list.
  //    c) For however many secondaries created in step b, minus 1, run d8 for each to merge in a
  //       canary class.
  // 2) Root module, non-minimal primary dex
  //    a) Perform matching of primary dex patterns, applying ClassNameFilter against jar entries.
  //    b) Run d8 to create the primary dex and N secondaries, with the given primary dex list
  //       created by step a.
  //    c) The first dex created by b is the primary, copy it to the designated output.
  //    d) For however many secondaries created in step b, run d8 for each to merge in a canary
  //       class.
  // 3) Root module, bootstrap dex files. This is quite similar to scenario #1, except that in a)
  //    potentially many dex files could be created for the pattern matching classes. The first will
  //    be output to the normal path of the primary dex, and the rest will be put into a separate
  //    directory. Execution will carry on as described in #1, except that naming conventions must
  //    honor a different number of existing dex files.
  // 4) Non-root module.
  //    a) Assert no given primary dex patterns and no minimize options.
  //    b) Run d8 to create N dexes, making a canary class as this run's primary dex list.
  //    c) For each dex file beyond the first, run d8 again to merge in a canary class.
  private void run() throws IOException {
    Optional<Integer> minSdkVersion =
        Optional.ofNullable(minSdkVersionString).map(Integer::parseInt);
    Path d8OutputDir = Files.createTempDirectory("d8_output_dir");
    Path canaryClassDirectory = Files.createTempDirectory("canary_classes");
    ImmutableList<Path> filesToDex =
        Files.readAllLines(Paths.get(filesToDexList)).stream()
            .map(Paths::get)
            .collect(ImmutableList.toImmutableList());
    ImmutableSet<Path> classpathFiles =
        classpathFilesList == null
            ? ImmutableSet.of()
            : Files.readAllLines(classpathFilesList).stream()
                .map(Paths::get)
                .collect(ImmutableSet.toImmutableSet());
    ImmutableSet<Path> classpath =
        ImmutableSet.copyOf(Sets.difference(classpathFiles, ImmutableSet.copyOf(filesToDex)));

    boolean deliberatelySculptingPrimaryDex = minimizePrimaryDex || enableBootstrapDexes;
    Path primaryDexClassNamesPath = Files.createTempFile("primary_dex_class_names", "txt");
    Path bootstrapDexOutputDir = null;
    if (bootstrapDexOutputDirString != null) {
      bootstrapDexOutputDir = Paths.get(bootstrapDexOutputDirString);
      Files.createDirectories(bootstrapDexOutputDir);
    }

    int moduleDexFilesEmitted = 0;
    if (!APKModule.isRootModule(module)) {
      Preconditions.checkState(primaryDexPatternsPathString == null);
      Preconditions.checkState(deobfuscatedPrimaryDexClassNamesPathString == null);
      Preconditions.checkState(primaryDexFilesToDexList == null);
      Preconditions.checkState(primaryDexString == null);
      Preconditions.checkState(!deliberatelySculptingPrimaryDex);
    } else {
      Preconditions.checkState(primaryDexString != null);
      ProguardTranslatorFactory proguardTranslatorFactory =
          ProguardTranslatorFactory.create(
              Optional.ofNullable(proguardConfigurationFileString).map(Paths::get),
              Optional.ofNullable(proguardMappingFileString).map(Paths::get),
              false);
      Function<String, String> deobfuscateFunction =
          proguardTranslatorFactory.createDeobfuscationFunction();
      if (deliberatelySculptingPrimaryDex) {
        Preconditions.checkState(primaryDexFilesToDexList != null);
        Preconditions.checkState(primaryDexPatternsPathString == null);
        ImmutableList<Path> primaryDexFilesToDex =
            Files.readAllLines(Paths.get(primaryDexFilesToDexList)).stream()
                .map(Paths::get)
                .collect(ImmutableList.toImmutableList());
        Predicate<String> matchesAllFiles = f -> true;
        PrimaryDexClassNamesHolder primaryDexClassNamesHolder =
            getPrimaryDexClassNames(primaryDexFilesToDex, matchesAllFiles, deobfuscateFunction);
        Preconditions.checkState(classpath.isEmpty());
        classpath = ImmutableSet.copyOf(primaryDexFilesToDex);
        if (enableBootstrapDexes) {
          Preconditions.checkNotNull(
              bootstrapDexOutputDir,
              "Need to supply --bootstrap-dex-output-dir with --enable-bootstrap-dexes");
          Path d8OutputDirInitial = Files.createTempDirectory("d8_output_dir_bootstrap");
          // Similar as the below minimizePrimaryDex case, but generate potentially many dex files
          // for the base.apk file using the split up primary pattern matching jars (output path to
          // d8 is a dir instead of single file as above). However many .dex files created beyond
          // classes.dex will be used for naming of subsequent dex files.
          Path hackMainDexListForBootstrapRun =
              synthesizePrimaryDexClassNamesFileForDexes(primaryDexFilesToDex, "bootstrap");
          try {
            D8Utils.runD8Command(
                new D8Utils.D8DiagnosticsHandler(),
                d8OutputDirInitial,
                primaryDexFilesToDex,
                noOptimize ? EnumSet.of(D8Options.NO_OPTIMIZE) : EnumSet.noneOf(D8Options.class),
                Optional.of(hackMainDexListForBootstrapRun),
                Paths.get(androidJar),
                filesToDex,
                minSdkVersion);
          } catch (CompilationFailedException e) {
            throw new IOException(e);
          }
          // Since d8 is outputting to directory, copy individual files over.
          Path createdClassesDotDex = d8OutputDirInitial.resolve("classes.dex");
          Preconditions.checkState(Files.exists(createdClassesDotDex));
          moduleDexFilesEmitted = (int) Files.list(d8OutputDirInitial).count();
          Path primaryDexPath = Paths.get(primaryDexString);
          Files.move(createdClassesDotDex, primaryDexPath);
          // Create and fill the bootstrap dex dir. Perhaps it would be nicer if primary dex was
          // just a dir of many. classes.dex was already moved away.
          final int bootstrapDexFileCount = moduleDexFilesEmitted - 1;
          for (int i = 0; i < bootstrapDexFileCount; i++) {
            String dexName = String.format("classes%d.dex", i + 2);
            Files.move(d8OutputDirInitial.resolve(dexName), bootstrapDexOutputDir.resolve(dexName));
          }
        } else if (minimizePrimaryDex) {
          // d8 is not adhere directly to the main-dex-list when it is asked to sculpt a dex;
          // it drags in more classes than strictly listed (based on the assumed low min sdk to
          // comply with quirks of the Dalvik runtime).
          // Instead, we just pass the .class files that we want to be in the primary dex to d8
          // directly.
          try {
            D8Utils.runD8Command(
                new D8Utils.D8DiagnosticsHandler(),
                Paths.get(primaryDexString),
                primaryDexFilesToDex,
                noOptimize ? EnumSet.of(D8Options.NO_OPTIMIZE) : EnumSet.noneOf(D8Options.class),
                Optional.empty(),
                Paths.get(androidJar),
                filesToDex,
                minSdkVersion);
          } catch (CompilationFailedException e) {
            throw new IOException(e);
          }
          moduleDexFilesEmitted = 1;
        }
        // Common case for both minimize primary, and bootstrap primaries.
        Files.write(
            Paths.get(Objects.requireNonNull(deobfuscatedPrimaryDexClassNamesPathString)),
            primaryDexClassNamesHolder.deobfuscatedPrimaryDexClassNames);
      } else {
        List<String> primaryDexPatterns =
            Files.readAllLines(Paths.get(Objects.requireNonNull(primaryDexPatternsPathString)));
        ClassNameFilter primaryDexClassNameFilter =
            ClassNameFilter.fromConfiguration(primaryDexPatterns);
        Predicate<String> primaryDexPatternMatcher = primaryDexClassNameFilter::matches;
        PrimaryDexClassNamesHolder primaryDexClassNamesHolder =
            getPrimaryDexClassNames(filesToDex, primaryDexPatternMatcher, deobfuscateFunction);

        Preconditions.checkState(
            !primaryDexClassNamesHolder.primaryDexClassNames.isEmpty(),
            "No primary dex classes were specified! Please add primary_dex_patterns "
                + "to ensure that at least one class exists in the primary dex.");
        Files.write(primaryDexClassNamesPath, primaryDexClassNamesHolder.primaryDexClassNames);
        Files.write(
            Paths.get(Objects.requireNonNull(deobfuscatedPrimaryDexClassNamesPathString)),
            primaryDexClassNamesHolder.deobfuscatedPrimaryDexClassNames);
      }
    }

    int canariesCreated = 0;
    if (!APKModule.isRootModule(module) || deliberatelySculptingPrimaryDex) {
      // We're just creating secondary dexes, and if we don't pass a main-dex-list then d8
      // will try to put everything into a single dex. Create our first "canary class" and
      // put it in the main-dex-list.
      Path firstSecondaryDexCanaryClass = createCanaryClass(canaryClassDirectory, 0);
      canariesCreated = 1;
      filesToDex =
          new ImmutableList.Builder<Path>()
              .addAll(filesToDex)
              .add(firstSecondaryDexCanaryClass)
              .build();
      Files.write(
          primaryDexClassNamesPath,
          ImmutableList.of(
              canaryClassDirectory.relativize(firstSecondaryDexCanaryClass).toString()));
    }

    try {
      D8Utils.runD8Command(
          new D8Utils.D8DiagnosticsHandler(),
          d8OutputDir,
          filesToDex,
          getD8Options(),
          Optional.ofNullable(primaryDexClassNamesPath),
          Paths.get(androidJar),
          classpath,
          minSdkVersion);
    } catch (CompilationFailedException e) {
      throw new IOException(e);
    }

    Path secondaryDexesWithCanaries = Paths.get(secondaryDexOutputDirString);
    Files.createDirectories(secondaryDexesWithCanaries);

    Path createdClassesDotDex = d8OutputDir.resolve("classes.dex");
    Preconditions.checkState(Files.exists(createdClassesDotDex));
    if (APKModule.isRootModule(module) && !deliberatelySculptingPrimaryDex) {
      Path primaryDexPath = Paths.get(Objects.requireNonNull(primaryDexString));
      Files.move(createdClassesDotDex, primaryDexPath);
      moduleDexFilesEmitted = 1;
    } else {
      // This is either a non-root module (with a canary prepended), or the root module for whom
      // primary dex classes have already been taken care of. In either case, classes.dex is
      // actually the first secondary dex, so just copy it over.
      Files.move(
          createdClassesDotDex,
          secondaryDexesWithCanaries.resolve(
              getRawSecondaryDexName(module, moduleDexFilesEmitted, 0)));
      moduleDexFilesEmitted += 1;
    }

    // Our d8 command created N secondary dexes. Per convention, a "canary class" should exist in
    // each of them (for runtime invariants), so for each of them we create a canary class and then
    // re-run d8 with the original secondary dex and the canary class path. Note that in certain
    // situations above, a canary might already be created and manually used as a main-dex-list.
    // Note: the canary class does not contribute any fields or methods, so this shouldn't cause an
    // existing .dex file to overflow.
    createSecondaryDexOutputWithCanaries(
        d8OutputDir, moduleDexFilesEmitted, canariesCreated, minSdkVersion);
  }

  /**
   * D8 run will create N secondary dex files, but per convention canary classes should be added for
   * loading at runtime and integrity checks. For each given dex we create a canary class and then
   * re-run d8 with the original secondary dex and the canary class path. The canary class does not
   * contribute any fields or methods, so this shouldn't cause a dex to overflow.
   *
   * @param d8OutputDir Where to find the dex files.
   * @param moduleDexFilesEmitted How many dex files for the currently processed module were already
   *     created and moved to their appropriate output dir. This will form the naming of subsequent
   *     dex files moved by this function.
   * @param canariesCreated How many canary classes were created by the earlier stage.
   * @param minSdkVersion To pass to d8.
   * @throws IOException
   */
  private void createSecondaryDexOutputWithCanaries(
      Path d8OutputDir,
      int moduleDexFilesEmitted,
      int canariesCreated,
      Optional<Integer> minSdkVersion)
      throws IOException {
    Path secondaryDexesWithCanaries = Paths.get(secondaryDexOutputDirString);
    Files.createDirectories(secondaryDexesWithCanaries);
    Path canaryClassDirectory = Files.createTempDirectory("canary_classes");
    // Check if a previous step has already consumed the classes.dex file from this output dir. If
    // so, count beyond it.
    long secondaryDexCount = Files.list(d8OutputDir).count();
    for (int rawIndex = 0; rawIndex < secondaryDexCount; rawIndex++) {
      String secondaryDexName = getRawSecondaryDexName(module, moduleDexFilesEmitted, rawIndex);
      Path secondaryDexPath = secondaryDexesWithCanaries.resolve(secondaryDexName);
      Preconditions.checkState(
          !Files.exists(secondaryDexPath), "Path should not already exist: " + secondaryDexPath);

      Path rawSecondaryDexPath = d8OutputDir.resolve(String.format("classes%d.dex", rawIndex + 2));
      Preconditions.checkState(
          Files.exists(rawSecondaryDexPath), "Expected file to exist at: " + rawSecondaryDexPath);
      Path canaryClass = createCanaryClass(canaryClassDirectory, rawIndex + canariesCreated);
      try {
        D8Utils.runD8Command(
            new D8Utils.D8DiagnosticsHandler(),
            secondaryDexPath,
            ImmutableList.of(rawSecondaryDexPath, canaryClass),
            getD8Options(),
            Optional.empty(),
            Paths.get(androidJar),
            ImmutableList.of(),
            minSdkVersion);
      } catch (CompilationFailedException e) {
        throw new IOException(e);
      }
    }
  }

  private Path createCanaryClass(Path directory, int index) throws IOException {
    String className =
        CanaryUtils.getFullyQualifiedCanaryClassName(canaryClassName, index).replace('.', '/');
    byte[] canaryClassBytes = CanaryUtils.createCanaryClassByteCode(className);

    Path canaryClassPath = directory.resolve(className + ".class");
    Files.createDirectories(Objects.requireNonNull(canaryClassPath.getParent()));
    Files.write(canaryClassPath, canaryClassBytes);

    return canaryClassPath;
  }

  private String getRawSecondaryDexName(String module, int emittedCount, int index) {
    index += emittedCount;
    if (APKModule.isRootModule(module)) {
      Preconditions.checkState(emittedCount >= 1, "Expected to have generated primary dex already");
      return String.format("classes%d.dex", index + 1);
    } else if (index == 0) {
      return "classes.dex";
    } else {
      return String.format("classes%d.dex", index + 1);
    }
  }

  private static class PrimaryDexClassNamesHolder {
    private final ImmutableList<String> primaryDexClassNames;
    private final ImmutableList<String> deobfuscatedPrimaryDexClassNames;

    private PrimaryDexClassNamesHolder(
        ImmutableList<String> primaryDexClassNames,
        ImmutableList<String> deobfuscatedPrimaryDexClassNames) {
      this.primaryDexClassNames = primaryDexClassNames;
      this.deobfuscatedPrimaryDexClassNames = deobfuscatedPrimaryDexClassNames;
    }
  }

  private PrimaryDexClassNamesHolder getPrimaryDexClassNames(
      ImmutableList<Path> filesToDex,
      Predicate<String> primaryDexClassNameMatcher,
      Function<String, String> deobfuscateFunction)
      throws IOException {

    ImmutableList.Builder<String> primaryDexClassNames = ImmutableList.builder();
    ImmutableList.Builder<String> deobfuscatedPrimaryDexClassNames = ImmutableList.builder();

    for (Path fileToDex : filesToDex) {
      try (ZipFile zipFile = new ZipFile(fileToDex.toFile())) {
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
                      zipEntryName.substring(
                          0, zipEntryName.length() - CLASS_NAME_SUFFIX.length())));
          if (primaryDexClassNameMatcher.test(deobfuscatedClassName)) {
            primaryDexClassNames.add(zipEntryName);
            deobfuscatedPrimaryDexClassNames.add(deobfuscatedClassName + ".class");
          }
        }
      }
    }

    return new PrimaryDexClassNamesHolder(
        primaryDexClassNames.build(), deobfuscatedPrimaryDexClassNames.build());
  }

  /**
   * Look into the given jars to dex, returning the first entry name that looks like a class.
   *
   * @param filesToDex
   * @return
   */
  String extractFirstClassNameFromInputs(ImmutableList<Path> filesToDex) throws IOException {
    for (Path fileToDex : filesToDex) {
      try (ZipFile zipFile = new ZipFile(fileToDex.toFile())) {
        Enumeration<? extends ZipEntry> entries = zipFile.entries();
        while (entries.hasMoreElements()) {
          ZipEntry zipEntry = entries.nextElement();
          String zipEntryName = zipEntry.getName();
          // Ignore non-.class files.
          if (!zipEntryName.endsWith(CLASS_NAME_SUFFIX)) {
            continue;
          }
          return zipEntryName;
        }
      }
    }
    throw new IllegalStateException("No classes to dex");
  }

  /**
   * Most invocations to this tool neglect to pass in a minSdkVersion. As a result, d8 won't make
   * multiple dex files unless its given a main dex list. How we are using d8 is a bit ridiculous,
   * we pretend to be generating code as if it's the stone age without a modern minSdkVersion. So,
   * this helper function is a way to grab 1 class name from the inputs for the main dex list, so
   * that d8 has something to chew on.
   *
   * @param filesToDex The files that will be passed to d8.
   * @param desc Part of the file name for the temp txt file returned
   * @return A temp file path with one class name in it.
   */
  Path synthesizePrimaryDexClassNamesFileForDexes(ImmutableList<Path> filesToDex, String desc)
      throws IOException {
    Path primaryDexClassNamesPath = Files.createTempFile("primary_dex_class_names_" + desc, "txt");
    String className = extractFirstClassNameFromInputs(filesToDex);
    Files.write(primaryDexClassNamesPath, ImmutableList.of(className));
    return primaryDexClassNamesPath;
  }

  private Set<D8Options> getD8Options() {
    ImmutableSet.Builder<D8Options> d8OptionsBuilder = ImmutableSet.builder();
    if (noOptimize) {
      d8OptionsBuilder.add(D8Options.NO_OPTIMIZE);
    }
    d8OptionsBuilder.add(D8Options.MAXIMIZE_PRIMARY_DEX);

    return d8OptionsBuilder.build();
  }
}
