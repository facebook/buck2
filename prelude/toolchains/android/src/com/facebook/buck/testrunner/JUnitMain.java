/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.testrunner;

import static com.facebook.buck.testrunner.CheckDependency.requiresClass;

/**
 * Launcher for JUnit.
 *
 * <p>Expected arguments are:
 *
 * <ul>
 *   <li>(string) output directory
 *   <li>(long) default timeout in milliseconds (0 for no timeout)
 *   <li>(string) newline separated list of test selectors
 *   <li>(string...) fully-qualified names of test classes
 * </ul>
 *
 * <p>IMPORTANT! This class limits itself to types that are available in both the JDK and Android
 * Java API. The objective is to limit the set of files added to the ClassLoader that runs the test,
 * as not to interfere with the results of the test.
 */
public class JUnitMain {

  private JUnitMain() {
    // Launcher class.
  }

  public static void main(String[] args) {
    // Ensure that both junit and hamcrest are on the classpath
    requiresClass("junit", "org.junit.Test");
    requiresClass("hamcrest", "org.hamcrest.Description");

    JUnitRunner runner = new JUnitRunner();
    runner.parseArgs(args);
    runner.runAndExit();
  }
}
