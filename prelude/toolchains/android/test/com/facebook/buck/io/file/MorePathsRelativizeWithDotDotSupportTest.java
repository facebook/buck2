/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.io.file;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

import java.nio.file.Paths;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class MorePathsRelativizeWithDotDotSupportTest {

  @Parameters
  public static Object[][] data() {
    return new Object[][] {
      {"", "", ""},
      {"", "a", "a"},
      {"a", "", ".."},
      {"a", "a", ""},
      {"a/b/c", "a/d/e", "../../d/e"},
      {"a/b", "a", ".."},
      {"../a", "b", "../../b"},
      {"a", "../b", "../../b"},
      {"/a/b/c", "/a/b/d", "../d"}
    };
  }

  @Parameter(0)
  public String basePath;

  @Parameter(1)
  public String childPath;

  @Parameter(2)
  public String expected;

  @Test
  public void relativizeWithDotDotSupport() {
    assertThat(
        MorePaths.relativizeWithDotDotSupport(Paths.get(basePath), Paths.get(childPath)),
        equalTo(Paths.get(expected)));
  }
}
