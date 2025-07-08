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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItem;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.cd.model.java.AbiGenerationMode;
import com.facebook.buck.cd.model.java.BuildTargetValue.Type;
import com.facebook.buck.core.build.execution.context.IsolatedExecutionContext;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.core.BuildTargetValue;
import com.facebook.buck.step.StepExecutionResult;
import com.facebook.buck.step.StepExecutionResults;
import com.facebook.buck.step.TestExecutionContext;
import com.facebook.buck.testutil.TemporaryPaths;
import com.facebook.buck.testutil.TestConsole;
import com.facebook.buck.util.FakeProcess;
import com.facebook.buck.util.FakeProcessExecutor;
import com.google.common.base.Splitter;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSortedSet;
import java.io.File;
import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;
import javax.annotation.Nullable;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class JavacStepTest {

  @Rule public ExpectedException thrown = ExpectedException.none();
  @Rule public TemporaryPaths tmp = new TemporaryPaths();

  private String target;
  private BuildTargetValue buildTargetValue;
  private RelPath configuredBuckOut;
  private FakeJavac fakeJavac;
  private CompilerParameters compilerParameters;

  @Before
  public void setUp() {
    target = "//foo:bar";
    buildTargetValue = new BuildTargetValue(Type.LIBRARY, target);
    configuredBuckOut = RelPath.get("buck-out/v2");
    fakeJavac = new FakeJavac();
    compilerParameters =
        new CompilerParameters(
            ImmutableSortedSet.of(),
            ImmutableList.of(),
            ImmutableMap.of(),
            getCompilerOutputPaths(),
            AbiGenerationMode.CLASS,
            AbiGenerationMode.CLASS,
            false,
            null);
  }

  @Test
  public void successfulCompileDoesNotSendStdoutAndStderrToConsole() throws Exception {
    ResolvedJavacOptions javacOptions = getResolvedJavacOptions();

    JavacStep step =
        new JavacStep(
            fakeJavac,
            javacOptions,
            buildTargetValue,
            configuredBuckOut,
            getCompilerOutputPathsValue(),
            compilerParameters,
            null,
            null);

    FakeProcess fakeJavacProcess = new FakeProcess(0, "javac stdout\n", "javac stderr\n");

    AbsPath rootPath = tmp.getRoot();
    IsolatedExecutionContext executionContext =
        TestExecutionContext.newInstance(
            rootPath, new FakeProcessExecutor(p -> fakeJavacProcess, new TestConsole()));
    StepExecutionResult result = step.executeIsolatedStep(executionContext);

    // Note that we don't include stderr in the step result on success.
    assertThat(result, equalTo(StepExecutionResults.SUCCESS));
  }

  @Test
  public void failedCompileSendsStdoutAndStderrToConsole() throws Exception {
    ResolvedJavacOptions javacOptions = getResolvedJavacOptions();

    JavacStep step =
        new JavacStep(
            fakeJavac,
            javacOptions,
            buildTargetValue,
            configuredBuckOut,
            getCompilerOutputPathsValue(),
            compilerParameters,
            null,
            null);

    FakeProcess fakeJavacProcess = new FakeProcess(3, "javac stdout\n", "javac stderr\n");

    AbsPath rootPath = tmp.getRoot();
    IsolatedExecutionContext executionContext =
        TestExecutionContext.newInstance(
            rootPath, new FakeProcessExecutor(p -> fakeJavacProcess, new TestConsole()));
    StepExecutionResult result = step.executeIsolatedStep(executionContext);

    // JavacStep itself writes stdout to the console on error; we expect the Build class to write
    // the stderr stream returned in the StepExecutionResult
    assertThat(
        result,
        equalTo(
            new StepExecutionResult(
                StepExecutionResults.ERROR_EXIT_CODE, Optional.of("javac stderr\n"))));
  }

  @Test
  public void existingBootclasspathDirSucceeds() throws Exception {
    ResolvedJavacOptions javacOptions = getResolvedJavacOptions("/this-totally-exists");

    ClasspathChecker classpathChecker =
        new ClasspathChecker(
            "/", ":", Paths::get, dir -> true, file -> false, (path, glob) -> ImmutableSet.of());

    JavacStep step =
        new JavacStep(
            fakeJavac,
            javacOptions,
            buildTargetValue,
            configuredBuckOut,
            getCompilerOutputPathsValue(),
            classpathChecker,
            compilerParameters,
            null,
            null);

    FakeProcess fakeJavacProcess = new FakeProcess(0, "javac stdout\n", "javac stderr\n");

    AbsPath rootPath = tmp.getRoot();
    IsolatedExecutionContext executionContext =
        TestExecutionContext.newInstance(
            rootPath, new FakeProcessExecutor(p -> fakeJavacProcess, new TestConsole()));
    StepExecutionResult result = step.executeIsolatedStep(executionContext);

    assertThat(result, equalTo(StepExecutionResults.SUCCESS));
  }

  @Test
  public void bootclasspathResolvedToAbsolutePath() {
    ResolvedJavacOptions javacOptions =
        getResolvedJavacOptions("/this-totally-exists:relative-path");

    JavacStep step =
        new JavacStep(
            fakeJavac,
            javacOptions,
            buildTargetValue,
            configuredBuckOut,
            getCompilerOutputPathsValue(),
            compilerParameters,
            null,
            null);

    FakeProcess fakeJavacProcess = new FakeProcess(0, "javac stdout\n", "javac stderr\n");

    AbsPath rootPath = tmp.getRoot();
    IsolatedExecutionContext executionContext =
        TestExecutionContext.newInstance(
            rootPath, new FakeProcessExecutor(p -> fakeJavacProcess, new TestConsole()));

    String description = step.getIsolatedStepDescription(executionContext);
    List<String> options =
        Splitter.on(",")
            .trimResults()
            .splitToList(Splitter.on("Delimiter").splitToList(description).get(0));
    assertThat(options, hasItem("-bootclasspath"));
    int bootclasspathIndex = options.indexOf("-bootclasspath");
    String bootclasspath = options.get(bootclasspathIndex + 1);
    assertThat(bootclasspath, Matchers.not(Matchers.emptyOrNullString()));
    for (String path : Splitter.on(File.pathSeparator).split(bootclasspath)) {
      assertTrue(Paths.get(path).isAbsolute());
    }
  }

  @Test
  public void missingBootclasspathDirFailsWithError() throws Exception {
    ResolvedJavacOptions javacOptions = getResolvedJavacOptions("/no-such-dir");

    JavacStep step =
        new JavacStep(
            fakeJavac,
            javacOptions,
            buildTargetValue,
            configuredBuckOut,
            getCompilerOutputPathsValue(),
            compilerParameters,
            null,
            null);

    FakeProcess fakeJavacProcess = new FakeProcess(1, "javac stdout\n", "javac stderr\n");

    IsolatedExecutionContext executionContext =
        TestExecutionContext.newInstance(
            tmp.getRoot(), new FakeProcessExecutor(p -> fakeJavacProcess, new TestConsole()));
    thrown.expectMessage("Bootstrap classpath /no-such-dir contains no valid entries");
    step.executeIsolatedStep(executionContext);
  }

  private static ResolvedJavacOptions getResolvedJavacOptions() {
    return getResolvedJavacOptions(null);
  }

  private static ResolvedJavacOptions getResolvedJavacOptions(@Nullable String classpath) {
    return new ResolvedJavacOptions(
        Optional.ofNullable(classpath),
        ImmutableList.of() /* bootclasspathList */,
        JavacLanguageLevelOptions.DEFAULT,
        false /* debug */,
        false /* verbose */,
        JavacPluginParams.EMPTY /* javaAnnotationProcessorParams */,
        JavacPluginParams.EMPTY /* standardJavacPluginParams */,
        ImmutableList.of() /* extraArguments */,
        null /* systemImage */);
  }

  private CompilerOutputPathsValue getCompilerOutputPathsValue() {
    return CompilerOutputPathsValue.of(
        target, getCompilerOutputPaths(), getCompilerOutputPaths(), getCompilerOutputPaths());
  }

  private static CompilerOutputPaths getCompilerOutputPaths() {
    return new CompilerOutputPaths(
        RelPath.get("classesDir"),
        RelPath.get("outputJarDirPath"),
        Optional.empty(),
        RelPath.get("annotationPath"),
        RelPath.get("pathToSourcesList"),
        RelPath.get("workingDirectory"),
        Optional.empty());
  }
}
