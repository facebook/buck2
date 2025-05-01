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

import com.facebook.buck.core.filesystems.RelPath;
import org.junit.Test;

public class FileExtensionMatcherTest {

  @Test
  public void matchesPathsWithMatchingExtension() {
    FileExtensionMatcher matcher = FileExtensionMatcher.of("cpp");
    assertTrue(matcher.matches(RelPath.get("foo.cpp")));
  }

  @Test
  public void doesNotMatchPathsWithADifferentExtension() {
    FileExtensionMatcher matcher = FileExtensionMatcher.of("cpp");
    assertFalse(matcher.matches(RelPath.get("foo.java")));
  }
}
