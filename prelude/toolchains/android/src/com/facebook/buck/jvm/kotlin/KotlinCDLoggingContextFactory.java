/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin;

import com.facebook.buck.jvm.cd.command.kotlin.LanguageVersion;
import com.facebook.buck.jvm.kotlin.cd.analytics.ClasspathChangesParam;
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlinCDLoggingContext;
import com.facebook.buck.jvm.kotlin.cd.analytics.KotlincModeParam;
import com.facebook.buck.jvm.kotlin.cd.analytics.StepParam;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.ClasspathChanges;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode;

public class KotlinCDLoggingContextFactory {

  private KotlinCDLoggingContextFactory() {}

  public static KotlinCDLoggingContext create(
      KotlincStep kotlincStep, LanguageVersion languageVersion, KotlincMode kotlincMode) {
    return new KotlinCDLoggingContext(create(kotlincStep), languageVersion, create(kotlincMode));
  }

  private static KotlincModeParam create(KotlincMode kotlincMode) {
    if (kotlincMode instanceof KotlincMode.NonIncremental) {
      return KotlincModeParam.NonIncremental.INSTANCE;
    } else if (kotlincMode instanceof KotlincMode.Incremental) {
      return new KotlincModeParam.Incremental(
          create(((KotlincMode.Incremental) kotlincMode).getClasspathChanges()));
    } else {
      throw new IllegalArgumentException("Unsupported kotlinc mode: " + kotlincMode);
    }
  }

  private static ClasspathChangesParam create(ClasspathChanges classpathChanges) {
    if (classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler) {
      return ClasspathChangesParam.TO_BE_COMPUTED_BY_INCREMENTAL_COMPILER;
    } else if (classpathChanges instanceof ClasspathChanges.NoChanges) {
      return ClasspathChangesParam.NO_CHANGES;
    } else if (classpathChanges instanceof ClasspathChanges.Unknown) {
      return ClasspathChangesParam.UNKNOWN;
    } else {
      throw new IllegalArgumentException("Unsupported classpathChanges mode: " + classpathChanges);
    }
  }

  private static StepParam create(KotlincStep kotlincStep) {
    if (kotlincStep instanceof KosabiStubgenStep) {
      return StepParam.KOSABI_STUBGEN;
    } else if (kotlincStep instanceof Ksp1Step) {
      return StepParam.KSP1;
    } else if (kotlincStep instanceof KaptStep) {
      return StepParam.KAPT;
    } else {
      return StepParam.KOTLINC;
    }
  }
}
