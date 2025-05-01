/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.cd.model.java.AbiGenerationMode;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.core.BuildTargetValue;
import com.facebook.buck.jvm.java.abi.source.api.SourceOnlyAbiRuleInfoFactory;
import com.facebook.buck.util.Escaper;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSortedSet;
import java.util.function.Function;
import javax.annotation.Nullable;

/** Interface for a resolved javac tool. */
public interface ResolvedJavac {

  /** An escaper for arguments written to @argfiles. */
  Function<String, String> ARGFILES_ESCAPER = Escaper.javacEscaper();

  /** Prepares an invocation of the compiler with the given parameters. */
  ResolvedJavac.Invocation newBuildInvocation(
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
      @Nullable SourceOnlyAbiRuleInfoFactory ruleInfoFactory);

  String getDescription(ImmutableList<String> options, RelPath pathToSrcsList);

  /** Returns a short name of the tool */
  String getShortName();

  /** Enum that specify a type of java compiler. */
  enum Source {
    /** Shell out to the javac in the JDK */
    EXTERNAL,
    /** Run javac in-process, loading it from the JRE in which Buck is running. */
    JDK,
  }

  /** Interface that defines invocation object created during java compilation. */
  interface Invocation extends AutoCloseable {

    /**
     * Produces a source-only ABI jar. {@link #buildClasses} may not be called on an invocation on
     * which this has been called.
     */
    int buildSourceOnlyAbiJar() throws InterruptedException;

    /** Produces a source ABI jar. Must be called before {@link #buildClasses} */
    int buildSourceAbiJar() throws InterruptedException;

    int buildClasses() throws InterruptedException;

    @Override
    void close();
  }
}
