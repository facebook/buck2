/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android;

import com.facebook.buck.util.Console;
import com.facebook.buck.util.environment.EnvVariablesProvider;
import com.google.common.collect.ImmutableMap;

public class AdbExecutionContext {
  private final Console console;

  public AdbExecutionContext(Console console) {
    this.console = console;
  }

  public ImmutableMap<String, String> getEnvironment() {
    return EnvVariablesProvider.getSystemEnv();
  }

  public Console getConsole() {
    return console;
  }
}
