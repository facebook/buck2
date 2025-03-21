/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.testrunner;

import java.util.Optional;
import java.util.function.Predicate;
import org.junit.jupiter.api.extension.ConditionEvaluationResult;
import org.junit.jupiter.api.extension.ExecutionCondition;
import org.junit.jupiter.api.extension.ExtensionContext;

/**
 * To be registered as an extension at <code>
 * "META-INF/services/org.junit.jupiter.api.extension.Extension"</code>
 */
public class SkipTestCondition implements ExecutionCondition {

  private static Predicate<ExtensionContext> SKIP_FILTER;

  static void useSkipFilter(Predicate<ExtensionContext> filter) {
    SKIP_FILTER = filter;
  }

  static Predicate<ExtensionContext> getSkipFilter() {
    return Optional.ofNullable(SKIP_FILTER).orElse(c -> false);
  }

  /**
   * @param context execution context.
   * @return enabled or disabled if the execution is a dry-run.
   */
  @Override
  public ConditionEvaluationResult evaluateExecutionCondition(ExtensionContext context) {
    if (isTestExecution(context) && shouldSkip(context)) {
      return ConditionEvaluationResult.disabled("skip");
    }
    return ConditionEvaluationResult.enabled(null);
  }

  private boolean isTestExecution(ExtensionContext context) {
    return "MethodExtensionContext".equals(context.getClass().getSimpleName());
  }

  private boolean shouldSkip(ExtensionContext context) {
    return getSkipFilter().test(context);
  }
}
