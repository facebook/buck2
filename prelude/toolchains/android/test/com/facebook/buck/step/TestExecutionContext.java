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
import com.facebook.buck.util.ProcessExecutor;
import com.facebook.buck.util.environment.EnvVariablesProvider;

public class TestExecutionContext {

  private TestExecutionContext() {
    // Utility class.
  }

  // For test code, use a global class loader cache to avoid having to call ExecutionContext.close()
  // in each test case.
  private static final ClassLoaderCache testClassLoaderCache = new ClassLoaderCache();

  public static IsolatedExecutionContext newInstance(AbsPath root) {
    return newInstance(root, new FakeProcessExecutor());
  }

  public static IsolatedExecutionContext newInstance(
      AbsPath rootPath, ProcessExecutor processExecutor) {
    return new IsolatedExecutionContext(
        testClassLoaderCache,
        new TestConsole(),
        processExecutor,
        rootPath,
        EnvVariablesProvider.getSystemEnv());
  }
}
