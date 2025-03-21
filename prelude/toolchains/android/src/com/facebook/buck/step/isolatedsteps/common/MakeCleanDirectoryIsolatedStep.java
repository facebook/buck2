/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.step.isolatedsteps.common;

import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.step.isolatedsteps.IsolatedStep;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;

/**
 * Deletes the directory, if it exists, before creating it. {@link MakeCleanDirectoryIsolatedStep}
 * is preferable to {@link MkdirIsolatedStep} if the directory may contain many generated files and
 * we want to avoid the case where it could accidentally include generated files from a previous run
 * in Buck.
 *
 * <p>For example, for a directory of {@code .class} files, if the user deletes a {@code .java} file
 * that generated one of the {@code .class} files, the {@code .class} file corresponding to the
 * deleted {@code .java} file should no longer be there when {@code javac} is run again.
 */
public final class MakeCleanDirectoryIsolatedStep {

  public static ImmutableList<IsolatedStep> of(RelPath path) {
    return ImmutableList.of(
        new RmIsolatedStep(path, true, ImmutableSet.of()), new MkdirIsolatedStep(path));
  }

  private MakeCleanDirectoryIsolatedStep() {}
}
