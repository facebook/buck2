/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.abi.source;

import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiTest;
import java.io.IOException;
import java.util.Map;
import org.junit.runners.Parameterized;

/**
 * A base class for tests that compare the behavior of javac's implementation of Elements and
 * TypeMirrors to Buck's Tree-backed one.
 */
public abstract class CompilerTreeApiParameterizedTest extends CompilerTreeApiTest {
  private static final String JAVAC = "javac";
  private static final String TREES = "trees";

  @Parameterized.Parameter public String implementation;

  @Parameterized.Parameters(name = "{0}")
  public static Object[] getParameters() {
    return new Object[] {JAVAC, TREES};
  }

  @Override
  protected boolean useFrontendOnlyJavacTask() {
    return testingTrees();
  }

  protected boolean testingJavac() {
    return implementation.equals(JAVAC);
  }

  protected boolean testingTrees() {
    return implementation.equals(TREES);
  }

  protected void withClasspathForJavacOnly(Map<String, String> fileNamesToContents)
      throws IOException {
    if (testingTrees()) {
      return;
    }
    super.withClasspath(fileNamesToContents);
  }
}
