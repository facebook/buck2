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

import com.facebook.buck.core.build.execution.context.IsolatedExecutionContext;
import com.facebook.buck.jvm.kotlin.buildtools.snapshot.ClasspathSnapshotGenerator;
import com.facebook.buck.jvm.kotlin.buildtools.snapshot.SnapshotGranularity;
import com.facebook.buck.step.StepExecutionResult;
import com.facebook.buck.step.StepExecutionResults;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import java.io.IOException;
import java.nio.file.Path;

/**
 * An isolated step to generate a classpath snapshot for a given jar. In majority of cases snapshot
 * generation happens on buck2 rule layer, but there are cases when the snapshot is missing and we
 * need to generate it here. Exmples of such cases: - Whne the jar file in the classpath is a
 * prebuilt jar with bootstrap toolchain - When we add a jar to the classpath that is not a part of
 * the build graph (ex. kotlin-stdlib.jar)
 */
public class ClasspathSnapshotGeneratorStep implements IsolatedStep {
  private final Path inputJarPath;
  private final Path outputSnapshotPath;
  private final SnapshotGranularity snapshotGranularity;

  private ClasspathSnapshotGeneratorStep(
      Path inputJarPath, Path outputSnapshotPath, SnapshotGranularity snapshotGranularity) {
    this.inputJarPath = inputJarPath;
    this.outputSnapshotPath = outputSnapshotPath;
    this.snapshotGranularity = snapshotGranularity;
  }

  @Override
  public StepExecutionResult executeIsolatedStep(IsolatedExecutionContext context)
      throws IOException, InterruptedException {
    new ClasspathSnapshotGenerator(
            this.inputJarPath, this.outputSnapshotPath, this.snapshotGranularity)
        .run();
    return StepExecutionResults.SUCCESS;
  }

  @Override
  public String getIsolatedStepDescription(IsolatedExecutionContext context) {
    return String.format(
        "runs prelude//toolchains/android/src/com/facebook/buck/jvm/kotlin/buildtools:cp_snapshot_generator_binary"
            + " --input-jar %s --output-snapshot %s --granularity %s",
        this.inputJarPath, this.outputSnapshotPath, this.snapshotGranularity);
  }

  @Override
  public String getShortName() {
    return "genClasspathSnapshot";
  }

  public static ClasspathSnapshotGeneratorStep of(
      Path inputJarPath, Path outputSnapshotPath, SnapshotGranularity snapshotGranularity) {
    return new ClasspathSnapshotGeneratorStep(
        inputJarPath, outputSnapshotPath, snapshotGranularity);
  }
}
