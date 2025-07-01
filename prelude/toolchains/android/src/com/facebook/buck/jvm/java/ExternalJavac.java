/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java;

import static java.nio.charset.StandardCharsets.UTF_8;

import com.facebook.buck.cd.model.java.AbiGenerationMode;
import com.facebook.buck.core.exceptions.HumanReadableException;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.facebook.buck.jvm.core.BuildTargetValue;
import com.facebook.buck.jvm.java.abi.source.api.SourceOnlyAbiRuleInfoFactory;
import com.facebook.buck.util.ProcessExecutor;
import com.facebook.buck.util.ProcessExecutorParams;
import com.google.common.base.Joiner;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSortedSet;
import java.io.IOException;
import java.util.Collection;
import java.util.Optional;
import java.util.stream.Stream;
import javax.annotation.Nullable;

/** javac implemented in a separate binary. */
public class ExternalJavac {

  private static final Logger LOG = Logger.get(ExternalJavac.class);

  private static final int SUCCESS_EXIT_CODE = 0;
  private static final int ERROR_EXIT_CODE = 1;

  /** External resolved external javac tool. */
  public static class ResolvedExternalJavac implements ResolvedJavac {

    private final String shortName;
    private final ImmutableList<String> commandPrefix;
    // maybe make it configurable
    private final ImmutableList<String> commonJavacOptions =
        ImmutableList.of("-encoding", UTF_8.name().toLowerCase());

    public ResolvedExternalJavac(String shortName, ImmutableList<String> commandPrefix) {
      this.shortName = shortName;
      this.commandPrefix = commandPrefix;
    }

    @Override
    public String getDescription(ImmutableList<String> options, RelPath pathToSrcsList) {
      StringBuilder builder = new StringBuilder(getShortName());
      appendToBuilder(builder, commonJavacOptions);
      appendToBuilder(builder, options);
      builder.append(" @").append(pathToSrcsList);
      return builder.toString();
    }

    private void appendToBuilder(StringBuilder builder, Collection<String> collection) {
      String delimiter = " ";
      if (!collection.isEmpty()) {
        builder.append(delimiter);
        Joiner.on(delimiter).appendTo(builder, collection);
      }
    }

    @Override
    public String getShortName() {
      return shortName;
    }

    public ImmutableList<String> getCommandPrefix() {
      return commandPrefix;
    }

    @Override
    public Invocation newBuildInvocation(
        JavacExecutionContext context,
        BuildTargetValue invokingRule,
        CompilerOutputPathsValue compilerOutputPathsValue,
        ImmutableList<String> options,
        JavacPluginParams annotationProcessorParams,
        JavacPluginParams pluginParams,
        ImmutableSortedSet<RelPath> javaSourceFilePaths,
        RelPath pathToSrcsList,
        RelPath workingDirectory,
        boolean trackClassUsage,
        @Nullable JarParameters abiJarParameters,
        @Nullable JarParameters libraryJarParameters,
        AbiGenerationMode abiGenerationMode,
        AbiGenerationMode abiCompatibilityMode,
        @Nullable SourceOnlyAbiRuleInfoFactory ruleInfoFactory) {
      Preconditions.checkArgument(abiJarParameters == null);
      Preconditions.checkArgument(libraryJarParameters == null);
      Preconditions.checkArgument(
          abiGenerationMode == AbiGenerationMode.CLASS,
          "Cannot compile ABI jars with external javac");

      return new Invocation() {
        @Override
        public int buildSourceOnlyAbiJar() {
          throw new UnsupportedOperationException(
              "Cannot build source-only ABI jar with external javac.");
        }

        @Override
        public int buildSourceAbiJar() {
          throw new UnsupportedOperationException(
              "Cannot build source ABI jar with external javac.");
        }

        @Override
        public int buildClasses() throws InterruptedException {
          // For consistency with javax.tools.JavaCompiler, if no sources are specified, then do
          // nothing. Although it seems reasonable to treat this case as an error, we have a
          // situation in KotlincToJarStepFactory where we need to categorically add a JavacStep in
          // the event
          // that an annotation processor for the kotlin_library() dynamically generates some .java
          // files that need to be compiled. Often, the annotation processors will do no such thing
          // and the JavacStep that was added will have no work to do.
          ImmutableList<RelPath> expandedSources = getExpandedSources();
          if (expandedSources.isEmpty()) {
            return SUCCESS_EXIT_CODE;
          }

          ImmutableList.Builder<String> command = ImmutableList.builder();
          command.addAll(commandPrefix);

          Stream<String> commonOptionsStream = commonJavacOptions.stream();
          Stream<String> optionsStream = options.stream();
          Stream<String> sourcesStream = expandedSources.stream().map(Object::toString);
          Stream<String> stream =
              Stream.concat(commonOptionsStream, Stream.concat(optionsStream, sourcesStream));
          try {
            ProjectFilesystemUtils.writeLinesToPath(
                context.getRuleCellRoot(),
                () -> stream.map(ResolvedJavac.ARGFILES_ESCAPER).iterator(),
                pathToSrcsList.getPath());
            command.add("@" + pathToSrcsList);
          } catch (IOException e) {
            throw new HumanReadableException(
                e, "Unable to write list of args/sources to compile to %s file!", pathToSrcsList);
          }

          return runCommand(command.build());
        }

        private ImmutableList<RelPath> getExpandedSources() {
          try {
            return JavaPaths.extractArchivesAndGetPaths(
                context.getRuleCellRoot(), javaSourceFilePaths, workingDirectory.getPath());
          } catch (IOException e) {
            throw new HumanReadableException(
                "Unable to expand sources for %s into %s",
                invokingRule.getFullyQualifiedName(), workingDirectory);
          }
        }

        private int runCommand(ImmutableList<String> command) throws InterruptedException {
          LOG.info("Running javac command: %s", command);
          try {
            ProcessExecutorParams params =
                new ProcessExecutorParams(
                    command,
                    Optional.of(context.getEnvironment()),
                    Optional.of(context.getRuleCellRoot().getPath()));
            ProcessExecutor processExecutor = context.getProcessExecutor();
            ProcessExecutor.Result result = processExecutor.launchAndExecute(params);
            return result.getExitCode();
          } catch (IOException e) {
            e.printStackTrace(context.getStdErr());
          }
          return ERROR_EXIT_CODE;
        }

        @Override
        public void close() {
          // Nothing to do
        }
      };
    }
  }
}
