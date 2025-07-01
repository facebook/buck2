/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.testutil.integration;

import com.google.common.base.Splitter;
import java.io.File;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * Utility to locate the {@code testdata} directory relative to an integration test.
 *
 * <p>Integration tests will often have a sample project layout in a directory under a directory
 * named {@code testdata}. The name of the directory should correspond to the scenario being tested.
 */
public class TestDataHelper {

  private static final Splitter JAVA_PACKAGE_SPLITTER = Splitter.on('.');

  private static final String TEST_DIRECTORY =
      new File("toolchains/android/test").getAbsolutePath();

  private static final String TESTDATA_DIRECTORY_NAME = "testdata";

  /** Utility class: do not instantiate. */
  private TestDataHelper() {}

  public static Path getTestDataDirectory(Object testCase) {
    return getTestDataDirectory(testCase.getClass());
  }

  public static Path getTestDataDirectory(Class<?> testCaseClass) {
    String javaPackage = testCaseClass.getPackage().getName();
    List<String> parts = new ArrayList<>();
    for (String component : JAVA_PACKAGE_SPLITTER.split(javaPackage)) {
      parts.add(component);
    }

    parts.add(TESTDATA_DIRECTORY_NAME);
    String[] directories = parts.toArray(new String[0]);
    Path result = FileSystems.getDefault().getPath(TEST_DIRECTORY, directories);

    // If we're running this test in IJ, then this path doesn't exist. Fall back to one that does
    if (!Files.exists(result)) {
      result =
          Paths.get("fbcode/buck2/prelude/toolchains/android/test")
              .resolve(testCaseClass.getPackage().getName().replace('.', '/'))
              .resolve("testdata");
    }
    return result;
  }

  public static Path getTestDataScenario(Object testCase, String scenario) {
    return getTestDataDirectory(testCase).resolve(scenario);
  }
}
