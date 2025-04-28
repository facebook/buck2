/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.testrunner;

import com.facebook.buck.test.result.type.ResultType;

/**
 * Result of an individual test method in JUnit. Similar to {@link org.junit.runner.Result}, except
 * that it always corresponds to exactly one test method.
 */
final class TestResult {
  final String testClassName;
  final String testMethodName;
  final long runTime;
  final ResultType type;
  final /* @Nullable */ Throwable failure;
  final /* @Nullable */ String stdOut;
  final /* @Nullable */ String stdErr;

  public TestResult(
      String testClassName,
      String testMethodName,
      long runTime,
      ResultType type,
      /* @Nullable */ Throwable failure,
      /* @Nullable */ String stdOut,
      /* @Nullable */ String stdErr) {
    this.testClassName = testClassName;
    this.testMethodName = testMethodName;
    this.runTime = runTime;
    this.type = type;
    this.failure = failure;
    this.stdOut = stdOut;
    this.stdErr = stdErr;
  }

  public static TestResult forDryRun(String testClassName, String testMethodName) {
    return new TestResult(testClassName, testMethodName, 0, ResultType.DRY_RUN, null, null, null);
  }

  public static TestResult forExcluded(String testClassName, String testMethodName, String reason) {
    return new TestResult(
        testClassName, testMethodName, 0, ResultType.EXCLUDED, null, reason, null);
  }

  public static TestResult forDisabled(String testClassName, String testMethodName) {
    return new TestResult(testClassName, testMethodName, 0, ResultType.DISABLED, null, null, null);
  }

  public boolean isSuccess() {
    return type == ResultType.SUCCESS;
  }
}
