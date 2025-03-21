/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.io.file;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.nio.file.Paths;
import org.junit.Test;

public class GlobPatternMatcherTest {

  @Test
  public void matchesPathsUnderProvidedBasePath() {
    GlobPatternMatcher matcher = GlobPatternMatcher.of("foo/*");
    assertTrue(matcher.matches(Paths.get("foo").resolve("bar")));
  }

  @Test
  public void doesNotMatchPathsOutsideOfProvidedBasePath() {
    GlobPatternMatcher matcher = GlobPatternMatcher.of("foo/*");
    assertFalse(matcher.matches(Paths.get("not_relative_too_root")));
  }
}
