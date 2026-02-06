/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin;

import static com.facebook.buck.jvm.java.CompilerOutputPaths.getKotlinTempDepFilePath;
import static com.google.common.collect.Iterables.transform;

import com.facebook.buck.core.build.execution.context.IsolatedExecutionContext;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.jvm.cd.command.kotlin.LanguageVersion;
import com.facebook.buck.jvm.core.BuildTargetValue;
import com.facebook.buck.jvm.java.CompilerOutputPaths;
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDAnalytics;
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDLoggingContext;
import com.facebook.buck.jvm.kotlin.kotlinc.Kotlinc;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode;
import com.facebook.buck.jvm.kotlin.plugin.PluginLoader;
import com.facebook.buck.step.StepExecutionResult;
import com.facebook.buck.step.StepExecutionResults;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import com.facebook.buck.util.CapturingPrintStream;
import com.facebook.buck.util.Verbosity;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.Optional;
import javax.annotation.Nullable;

/** Kotlin compile Step */
public class KotlincStep implements IsolatedStep {
  private static final Logger LOG = Logger.get(KotlincStep.class);

  private static final String CLASSPATH_FLAG = "-classpath";
  private static final String DESTINATION_FLAG = "-d";
  private static final String INCLUDE_RUNTIME_FLAG = "-include-runtime";
  private static final String EXCLUDE_REFLECT = "-no-reflect";
  private static final String X_PLUGIN_ARG = "-Xplugin=";
  private static final String PLUGIN = "-P";

  private static final int EXPECTED_SOURCE_ONLY_ABI_EXIT_CODE = 2;

  private final Kotlinc kotlinc;
  private final ImmutableList<AbsPath> combinedClassPathEntries;
  private final ImmutableList<AbsPath> kotlinHomeLibraries;
  private final Path outputDirectory;
  private final ImmutableList<String> extraArguments;
  private final ImmutableList<String> verboseModeOnlyExtraArguments;
  private final ImmutableSortedSet<RelPath> sourceFilePaths;
  private final Path pathToSrcsList;
  private final RelPath reportDirPath;
  private final BuildTargetValue invokingRule;
  private final CompilerOutputPaths outputPaths;
  private final boolean trackClassUsage;
  private final RelPath configuredBuckOut;
  private final ImmutableMap<String, AbsPath> resolvedKosabiPluginOptionPath;
  private final @Nullable String kosabiEarlyTerminationMessagePrefix;
  private final boolean kosabiShouldEnableMixedCompilation;
  private final ImmutableList<AbsPath> sourceOnlyAbiClasspath;
  private final boolean verifySourceOnlyAbiConstraints;
  private ImmutableList<IsolatedStep> postKotlinCompilationFailureSteps;
  private final Optional<AbsPath> depTrackerPath;
  private final KotlincMode kotlincMode;
  private final KotlinCDAnalytics kotlinCDAnalytics;
  private final LanguageVersion languageVersion;
  private final boolean shouldKosabiJvmAbiGenUseK2;

  KotlincStep(
      BuildTargetValue invokingRule,
      Path outputDirectory,
      ImmutableSortedSet<RelPath> sourceFilePaths,
      Path pathToSrcsList,
      ImmutableList<AbsPath> combinedClassPathEntries,
      ImmutableList<AbsPath> kotlinHomeLibraries,
      RelPath reportDirPath,
      Kotlinc kotlinc,
      ImmutableList<String> extraArguments,
      ImmutableList<String> verboseModeOnlyExtraArguments,
      CompilerOutputPaths outputPaths,
      boolean trackClassUsage,
      RelPath configuredBuckOut,
      ImmutableMap<String, AbsPath> resolvedKosabiPluginOptionPath,
      @Nullable String kosabiEarlyTerminationMessagePrefix,
      boolean kosabiShouldEnableMixedCompilation,
      ImmutableList<AbsPath> sourceOnlyAbiClasspath,
      boolean verifySourceOnlyAbiConstraints,
      ImmutableList<IsolatedStep> postKotlinCompilationFailureSteps,
      Optional<AbsPath> depTrackerPath,
      KotlincMode kotlincMode,
      KotlinCDAnalytics kotlinCDAnalytics,
      LanguageVersion languageVersion,
      boolean shouldKosabiJvmAbiGenUseK2) {
    this.invokingRule = invokingRule;
    this.outputDirectory = outputDirectory;
    this.sourceFilePaths = sourceFilePaths;
    this.pathToSrcsList = pathToSrcsList;
    this.reportDirPath = reportDirPath;
    this.kotlinc = kotlinc;
    this.combinedClassPathEntries = combinedClassPathEntries;
    this.kotlinHomeLibraries = kotlinHomeLibraries;
    this.extraArguments = extraArguments;
    this.verboseModeOnlyExtraArguments = verboseModeOnlyExtraArguments;
    this.outputPaths = outputPaths;
    this.trackClassUsage = trackClassUsage;
    this.configuredBuckOut = configuredBuckOut;
    this.resolvedKosabiPluginOptionPath = resolvedKosabiPluginOptionPath;
    this.kosabiEarlyTerminationMessagePrefix = kosabiEarlyTerminationMessagePrefix;
    this.kosabiShouldEnableMixedCompilation = kosabiShouldEnableMixedCompilation;
    this.sourceOnlyAbiClasspath = sourceOnlyAbiClasspath;
    this.verifySourceOnlyAbiConstraints = verifySourceOnlyAbiConstraints;
    this.postKotlinCompilationFailureSteps = postKotlinCompilationFailureSteps;
    this.depTrackerPath = depTrackerPath;
    this.kotlincMode = kotlincMode;
    this.kotlinCDAnalytics = kotlinCDAnalytics;
    this.languageVersion = languageVersion;
    this.shouldKosabiJvmAbiGenUseK2 = shouldKosabiJvmAbiGenUseK2;
  }

  @Override
  public String getShortName() {
    return getKotlinc().getShortName();
  }

  @Override
  public StepExecutionResult executeIsolatedStep(IsolatedExecutionContext context) {
    ImmutableList<String> compilerOptions = getOptions(context, combinedClassPathEntries);
    Verbosity verbosity =
        context.getVerbosity().isSilent() ? Verbosity.STANDARD_INFORMATION : context.getVerbosity();
    try (CapturingPrintStream stdout = new CapturingPrintStream();
        CapturingPrintStream stderr = new CapturingPrintStream();
        IsolatedExecutionContext firstOrderContext =
            context.createSubContext(stdout, stderr, Optional.of(verbosity))) {

      KotlinCDLoggingContext loggingContext =
          KotlinCDLoggingContextFactory.create(this, languageVersion, kotlincMode);
      Instant compilationStart = Instant.now();

      int declaredDepsBuildResult =
          kotlinc.buildWithClasspath(
              firstOrderContext,
              invokingRule,
              compilerOptions,
              kotlinHomeLibraries,
              sourceFilePaths,
              pathToSrcsList,
              Optional.of(outputPaths.getWorkingDirectory().getPath()),
              context.getRuleCellRoot(),
              kotlincMode,
              loggingContext);

      Instant compilationEnd = Instant.now();
      Duration compilationDuration = Duration.between(compilationStart, compilationEnd);
      loggingContext.addExtras(
          this.getClass().getSimpleName(),
          "Kotlinc step duration: " + compilationDuration.toMillis() + " ms");
      kotlinCDAnalytics.log(loggingContext);

      String firstOrderStderr = stderr.getContentsAsString(StandardCharsets.UTF_8);
      Optional<String> returnedStderr;

      // We're generating Kotlin source-only-abi with Kosabi, a set of Kotlin compiler
      // plugins.
      // see `Kosabi.java`
      //
      // `jvm-abi-gen` is one of Kosabi plugins responsible for ABI class files
      // generation.
      // `jvm-abi-gen` could pass only the Kotlin Frontend Compiler stage, thus it's
      // intentionally
      // throws an Internal Compiler Error and exits with the corresponding exit code.
      //
      // EXPECTED_SOURCE_ONLY_ABI_EXIT_CODE is Kotlin compiler Internal Error code.
      // We should treat Internal Compiler Error in source-only-abi as an abi-generation
      // Success.
      // In addition to an exit code we're expecting a specific `firstOrderStderr` message
      // We both need an exit code and a message to prevent cache poisoning in cases when
      // Kotlin compiler had a legit Internal Error
      boolean shouldCheckSourceOnlyAbiErrorMessage = kosabiEarlyTerminationMessagePrefix != null;

      boolean isExpectedSourceOnlyAbiTermination =
          !shouldCheckSourceOnlyAbiErrorMessage
              || validateEarlyTerminationErrorMessage(context, firstOrderStderr);

      if (invokingRule.isSourceOnlyAbi()
          && declaredDepsBuildResult == EXPECTED_SOURCE_ONLY_ABI_EXIT_CODE
          && isExpectedSourceOnlyAbiTermination) {
        declaredDepsBuildResult = StepExecutionResults.SUCCESS_EXIT_CODE;
        returnedStderr = Optional.empty();
      } else if (declaredDepsBuildResult != StepExecutionResults.SUCCESS_EXIT_CODE) {
        for (IsolatedStep step : postKotlinCompilationFailureSteps) {
          LOG.debug("Executing step [%s] after KotlinC failure", step.getShortName());
          step.executeIsolatedStep(context);
        }
        returnedStderr = Optional.of(firstOrderStderr);
      } else {
        returnedStderr = Optional.empty();
      }

      // TODO: add used-classes support for Kosabi 2.0 T232722163
      if (declaredDepsBuildResult == StepExecutionResults.SUCCESS_EXIT_CODE
          && trackClassUsage
          && !shouldKosabiJvmAbiGenUseK2) {
        AbsPath ruleCellRoot = context.getRuleCellRoot();
        RelPath outputJarDirPath = outputPaths.getOutputJarDirPath();
        ClassUsageFileWriterFactory.create(kotlincMode)
            .writeFile(
                KotlinClassUsageHelper.getClassUsageData(reportDirPath, ruleCellRoot),
                CompilerOutputPaths.getKotlinDepFilePath(outputJarDirPath),
                ruleCellRoot,
                configuredBuckOut);
      }

      return new StepExecutionResult(declaredDepsBuildResult, returnedStderr);
    } catch (IOException | InterruptedException e) {
      throw new RuntimeException(e);
    }
  }

  @VisibleForTesting
  Kotlinc getKotlinc() {
    return kotlinc;
  }

  @Override
  public String getIsolatedStepDescription(IsolatedExecutionContext context) {
    return getKotlinc()
        .getDescription(
            getOptions(context, getClasspathEntries()), sourceFilePaths, pathToSrcsList);
  }

  /**
   * Returns a list of command-line options to pass to javac. These options reflect the
   * configuration of this javac command.
   *
   * @param context the ExecutionContext with in which javac will run
   * @return list of String command-line options.
   */
  @VisibleForTesting
  ImmutableList<String> getOptions(
      IsolatedExecutionContext context, ImmutableList<AbsPath> buildClasspathEntries) {
    ImmutableList.Builder<String> builder = ImmutableList.builder();

    AbsPath ruleCellRoot = context.getRuleCellRoot();

    if (outputDirectory != null) {
      builder.add(DESTINATION_FLAG, ruleCellRoot.resolve(outputDirectory).toString());
    }

    if (invokingRule.isSourceOnlyAbi()) {
      configureSourceOnlyOptions(builder, languageVersion, ruleCellRoot);
    } else if (invokingRule.isSourceAbi()) {
      throw new Error("Source ABI flavor is not supported for Kotlin targets");
    } else if (!buildClasspathEntries.isEmpty()) {
      addClasspath(builder, buildClasspathEntries);
    }

    // We expect Kosabi/Applicability to generate a compilation error if
    // a library target verification fails.
    // User will see a broken compilation with the following message:
    // Kosabi/Applicability FAILED on this target ...
    if (verifySourceOnlyAbiConstraints && invokingRule.isLibraryJar()) {
      if (resolvedKosabiPluginOptionPath.containsKey(
          KosabiConfig.PROPERTY_KOSABI_APPLICABILITY_PLUGIN)) {
        AbsPath applicabilityPlugin =
            resolvedKosabiPluginOptionPath.get(KosabiConfig.PROPERTY_KOSABI_APPLICABILITY_PLUGIN);
        builder.add(X_PLUGIN_ARG + applicabilityPlugin);
      }
    }

    builder.add(INCLUDE_RUNTIME_FLAG);
    builder.add(EXCLUDE_REFLECT);

    // TODO: add used-classes support for Kosabi 2.0 T232722163
    if (trackClassUsage && !shouldKosabiJvmAbiGenUseK2) {
      depTrackerPath.ifPresentOrElse(
          // New plugin, runtime dependency
          path -> {
            builder.add(X_PLUGIN_ARG + path);
          },
          // Fallback to a Buck internal plugin
          () -> builder.add(X_PLUGIN_ARG + PluginLoader.DEP_TRACKER_KOTLINC_PLUGIN_JAR_PATH));
      builder.add(PLUGIN);
      builder.add(
          "plugin:buck_deps_tracker:out="
              + ruleCellRoot.resolve(getKotlinTempDepFilePath(reportDirPath)));
    }

    if (!extraArguments.isEmpty()) {
      for (String extraArgument : extraArguments) {
        if (!extraArgument.isEmpty()) {
          builder.add(extraArgument);
        }
      }
    }

    if (languageVersion.getSupportsLanguageVersion()) {
      builder.add(languageVersion.getCompilerArgs());
    }

    if (context.getVerbosity().shouldUseVerbosityFlagIfAvailable()
        && !verboseModeOnlyExtraArguments.isEmpty()) {
      for (String extraArgument : verboseModeOnlyExtraArguments) {
        if (!extraArgument.isEmpty()) {
          builder.add(extraArgument);
        }
      }
    }

    return builder.build();
  }

  protected void configureSourceOnlyOptions(
      ImmutableList.Builder<String> builder,
      LanguageVersion languageVersion,
      AbsPath ruleCellRoot) {
    if (languageVersion.getSupportsK2()
        && resolvedKosabiPluginOptionPath.containsKey(
            KosabiConfig.PROPERTY_KOSABI_STUBS_GEN_K2_PLUGIN)) {
      AbsPath stubPlugin =
          resolvedKosabiPluginOptionPath.get(KosabiConfig.PROPERTY_KOSABI_STUBS_GEN_K2_PLUGIN);
      builder.add(X_PLUGIN_ARG + stubPlugin);
    } else if (resolvedKosabiPluginOptionPath.containsKey(
        KosabiConfig.PROPERTY_KOSABI_STUBS_GEN_PLUGIN)) {
      AbsPath stubPlugin =
          resolvedKosabiPluginOptionPath.get(KosabiConfig.PROPERTY_KOSABI_STUBS_GEN_PLUGIN);
      builder.add(X_PLUGIN_ARG + stubPlugin);
    }

    if (shouldKosabiJvmAbiGenUseK2) {
      if (!resolvedKosabiPluginOptionPath.containsKey(
          KosabiConfig.PROPERTY_KOSABI_JVM_ABI_GEN_K2_PLUGIN)) {
        throw new RuntimeException("KosabiJvmAbiGenK2Plugin is not provided");
      }
      AbsPath jvmAbiPlugin =
          resolvedKosabiPluginOptionPath.get(KosabiConfig.PROPERTY_KOSABI_JVM_ABI_GEN_K2_PLUGIN);
      builder.add(X_PLUGIN_ARG + jvmAbiPlugin);
      builder.add(PLUGIN);
      builder.add(
          "plugin:com.facebook.k2.jvm.abi.gen:outputDir="
              + ruleCellRoot.resolve(outputDirectory).toString());
    } else {
      if (resolvedKosabiPluginOptionPath.containsKey(
          KosabiConfig.PROPERTY_KOSABI_JVM_ABI_GEN_PLUGIN)) {
        AbsPath jvmAbiPlugin =
            resolvedKosabiPluginOptionPath.get(KosabiConfig.PROPERTY_KOSABI_JVM_ABI_GEN_PLUGIN);
        builder.add(X_PLUGIN_ARG + jvmAbiPlugin);
        builder.add(PLUGIN);
        builder.add("plugin:com.facebook.jvm.abi.gen:outputDir=" + outputPaths.getClassesDir());
        builder.add(PLUGIN);
        builder.add("plugin:com.facebook.jvm.abi.gen:earlyTermination=true");
        // Kosabi can only works with legacy jvm abi gen which use AnalysisHandlerExtension
        builder.add(PLUGIN);
        builder.add("plugin:com.facebook.jvm.abi.gen:useLegacyAbiGen=true");
        // Enable Mixed compilation if KspAnnotationProcessors are supported
        if (kosabiShouldEnableMixedCompilation) {
          builder.add(PLUGIN);
          builder.add("plugin:com.facebook.jvm.abi.gen:enableMixedCompilation=true");
        }
      }
      if (resolvedKosabiPluginOptionPath.containsKey(
          KosabiConfig.PROPERTY_KOSABI_SOURCE_MODIFIER_PLUGIN)) {
        AbsPath sourceModifierPlugin =
            resolvedKosabiPluginOptionPath.get(KosabiConfig.PROPERTY_KOSABI_SOURCE_MODIFIER_PLUGIN);
        builder.add(X_PLUGIN_ARG + sourceModifierPlugin);
      }
    }

    addClasspath(builder, this.sourceOnlyAbiClasspath);
  }

  private void addClasspath(ImmutableList.Builder<String> builder, Iterable<AbsPath> pathElements) {
    builder.add(
        CLASSPATH_FLAG,
        Joiner.on(File.pathSeparator)
            .join(transform(pathElements, path -> path.getPath().toString())));
  }

  private boolean validateEarlyTerminationErrorMessage(
      IsolatedExecutionContext context, String fullErrorMessage) {
    if (fullErrorMessage.isEmpty()) return false;

    int earlyTerminationMessageIndex =
        fullErrorMessage.indexOf(kosabiEarlyTerminationMessagePrefix);
    if (earlyTerminationMessageIndex >= 0) {
      return true;
    }
    return false;
  }

  /**
   * @return The classpath entries used to invoke javac.
   */
  @VisibleForTesting
  ImmutableList<AbsPath> getClasspathEntries() {
    return combinedClassPathEntries;
  }
}
