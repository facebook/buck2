/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.core.util.immutables.BuckStyleValueWithBuilder;
import com.facebook.buck.jvm.java.version.JavaVersion;
import org.immutables.value.Value;

@BuckStyleValueWithBuilder
public abstract class JavacLanguageLevelOptions {

  public static final JavacLanguageLevelOptions DEFAULT =
      JavacLanguageLevelOptions.builder().build();

  // Default combined source and target level.
  public static final String TARGETED_JAVA_VERSION = "7";

  @Value.Default
  protected String getSourceLevel() {
    return TARGETED_JAVA_VERSION;
  }

  @Value.Default
  protected String getTargetLevel() {
    return TARGETED_JAVA_VERSION;
  }

  @Value.Derived
  public JavaVersion getSourceLevelValue() {
    return JavaVersion.toJavaLanguageVersion(getSourceLevel());
  }

  @Value.Derived
  public JavaVersion getTargetLevelValue() {
    return JavaVersion.toJavaLanguageVersion(getTargetLevel());
  }

  public static Builder builder() {
    return new Builder();
  }

  public static class Builder extends ImmutableJavacLanguageLevelOptions.Builder {}
}
