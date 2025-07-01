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

import com.facebook.buck.cd.model.java.AbiGenerationMode;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.core.BuildTargetValue;
import com.facebook.buck.jvm.java.abi.source.api.SourceOnlyAbiRuleInfoFactory;
import com.facebook.buck.util.ProcessExecutorParams;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSortedSet;
import java.io.IOException;
import javax.annotation.Nullable;

/** Fake implementation of {@link ResolvedJavac} for tests. */
public class FakeJavac implements ResolvedJavac {

  @Override
  public ResolvedJavac.Invocation newBuildInvocation(
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
    return new ResolvedJavac.Invocation() {
      @Override
      public int buildSourceOnlyAbiJar() {
        throw new UnsupportedOperationException();
      }

      @Override
      public int buildSourceAbiJar() {
        throw new UnsupportedOperationException();
      }

      @Override
      public int buildClasses() throws InterruptedException {
        try {
          return context
              .getProcessExecutor()
              .launchAndExecute(ProcessExecutorParams.ofCommand("javac"))
              .getExitCode();
        } catch (IOException e) {
          return 1;
        }
      }

      @Override
      public void close() {
        // Nothing to do
      }
    };
  }

  @Override
  public String getDescription(ImmutableList<String> options, RelPath pathToSrcsList) {
    return String.format("fakeJavac %s %s", options, pathToSrcsList);
  }

  @Override
  public String getShortName() {
    throw new UnsupportedOperationException();
  }
}
