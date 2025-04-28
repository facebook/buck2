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
import com.facebook.buck.core.build.execution.context.IsolatedExecutionContext;
import com.facebook.buck.core.exceptions.HumanReadableException;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.facebook.buck.jvm.java.abi.StubJar;
import com.facebook.buck.step.StepExecutionResult;
import com.facebook.buck.step.StepExecutionResults;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import com.google.common.base.Preconditions;
import java.io.IOException;
import java.nio.file.Path;
import javax.annotation.Nullable;

/** Calculates class abi from the library.jar */
public class CalculateClassAbiStep implements IsolatedStep {

  private final RelPath binaryJar;
  @Nullable private final RelPath existingAbiJar;
  private final RelPath abiJar;
  private final AbiGenerationMode compatibilityMode;

  public CalculateClassAbiStep(
      RelPath binaryJar,
      @Nullable RelPath existingAbiJar,
      RelPath abiJar,
      AbiGenerationMode compatibilityMode) {
    this.binaryJar = binaryJar;
    this.existingAbiJar = existingAbiJar;
    this.abiJar = abiJar;
    this.compatibilityMode = compatibilityMode;
  }

  @Override
  public StepExecutionResult executeIsolatedStep(IsolatedExecutionContext context)
      throws IOException {
    AbsPath ruleCellRoot = context.getRuleCellRoot();
    AbsPath output = toAbsOutputPath(ruleCellRoot, abiJar);

    try {
      StubJar stubJar =
          new StubJar(ruleCellRoot.resolve(binaryJar), false)
              .setCompatibilityMode(compatibilityMode);
      if (existingAbiJar != null
          && ProjectFilesystemUtils.exists(ruleCellRoot, existingAbiJar.getPath())) {
        stubJar = stubJar.setExistingAbiJar(ruleCellRoot.resolve(existingAbiJar));
      }
      stubJar.writeTo(output);
    } catch (IllegalArgumentException e) {
      throw new HumanReadableException(e, "Failed to calculate ABI for %s.", binaryJar);
    }

    return StepExecutionResults.SUCCESS;
  }

  private AbsPath toAbsOutputPath(AbsPath root, RelPath relativeOutputPath) throws IOException {
    Path outputPath = ProjectFilesystemUtils.getPathForRelativePath(root, relativeOutputPath);
    Preconditions.checkState(
        !ProjectFilesystemUtils.exists(root, outputPath),
        "Output file already exists: %s",
        relativeOutputPath);

    if (outputPath.getParent() != null
        && !ProjectFilesystemUtils.exists(root, outputPath.getParent())) {
      ProjectFilesystemUtils.createParentDirs(root, outputPath);
    }
    return root.resolve(relativeOutputPath);
  }

  @Override
  public String getShortName() {
    return "class_abi";
  }

  @Override
  public String getIsolatedStepDescription(IsolatedExecutionContext context) {
    return String.format("%s %s", getShortName(), binaryJar);
  }
}
