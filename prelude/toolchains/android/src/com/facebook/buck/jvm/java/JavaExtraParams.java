/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.core.util.immutables.BuckStyleValue;

/** Extra params instance used to create Java related compile build steps */
@BuckStyleValue
public abstract class JavaExtraParams implements CompileToJarStepFactory.ExtraParams {

  public abstract ResolvedJavacOptions getResolvedJavacOptions();

  public abstract boolean isAddAnnotationPath();

  public static JavaExtraParams of(ResolvedJavacOptions resolvedJavacOptions) {
    return ImmutableJavaExtraParams.ofImpl(resolvedJavacOptions, true);
  }

  public static JavaExtraParams of(
      ResolvedJavacOptions resolvedJavacOptions, boolean addAnnotationPath) {
    return ImmutableJavaExtraParams.ofImpl(resolvedJavacOptions, addAnnotationPath);
  }
}
