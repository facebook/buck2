/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.step;

import com.facebook.buck.core.build.execution.context.IsolatedExecutionContext;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.testutil.TestConsole;
import com.facebook.buck.util.ClassLoaderCache;
import com.facebook.buck.util.FakeProcessExecutor;
import com.facebook.buck.util.environment.EnvVariablesProvider;
import java.nio.file.Paths;

public class TestExecutionContext {

  private TestExecutionContext() {
    // Utility class.
  }

  // For test code, use a global class loader cache to avoid having to call ExecutionContext.close()
  // in each test case.
  private static final ClassLoaderCache testClassLoaderCache = new ClassLoaderCache();

  public static IsolatedExecutionContext.Builder newBuilder() {
    AbsPath rootPath = AbsPath.of(Paths.get(".").toAbsolutePath()).normalize();
    return IsolatedExecutionContext.builder()
        .setConsole(new TestConsole())
        .setEnvironment(EnvVariablesProvider.getSystemEnv())
        .setClassLoaderCache(testClassLoaderCache)
        .setProcessExecutor(new FakeProcessExecutor())
        .setRuleCellRoot(rootPath);
  }

  public static IsolatedExecutionContext newInstance() {
    return newBuilder().build();
  }

  public static IsolatedExecutionContext newInstance(AbsPath root) {
    return newBuilder().setRuleCellRoot(root).build();
  }
}
