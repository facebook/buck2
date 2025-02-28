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
import com.facebook.buck.core.util.immutables.BuckStyleValueWithBuilder;
import com.facebook.buck.jvm.java.abi.source.api.SourceOnlyAbiRuleInfoFactory;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;
import javax.annotation.Nullable;
import org.immutables.value.Value;

@BuckStyleValueWithBuilder
public abstract class CompilerParameters {
  @Value.Default
  public ImmutableSortedSet<RelPath> getSourceFilePaths() {
    return ImmutableSortedSet.of();
  }

  @Value.Default
  public ImmutableList<RelPath> getClasspathEntries() {
    return ImmutableList.of();
  }

  @Value.Default
  public ImmutableMap<RelPath, RelPath> getClasspathSnapshots() {
    return ImmutableMap.of();
  }

  public abstract CompilerOutputPaths getOutputPaths();

  @Value.Default
  public AbiGenerationMode getAbiGenerationMode() {
    return AbiGenerationMode.CLASS;
  }

  @Value.Default
  public AbiGenerationMode getAbiCompatibilityMode() {
    return getAbiGenerationMode();
  }

  @Value.Default
  public boolean shouldTrackClassUsage() {
    return false;
  }

  @Value.Default
  public boolean shouldTrackJavacPhaseEvents() {
    return false;
  }

  @Nullable
  public abstract SourceOnlyAbiRuleInfoFactory getSourceOnlyAbiRuleInfoFactory();

  public static ImmutableCompilerParameters.Builder builder() {
    return ImmutableCompilerParameters.builder();
  }
}
