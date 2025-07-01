/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.io.pathformat;

import static org.junit.Assert.assertEquals;

import com.facebook.buck.util.environment.Platform;
import java.nio.file.Paths;
import org.junit.Test;

public class PathFormatterTest {
  @Test
  public void toStringWithUnixSeparatorDefaultPath() {
    assertEquals("foo/bar", PathFormatter.pathWithUnixSeparators("foo/bar"));
    assertEquals("foo/bar", PathFormatter.pathWithUnixSeparators(Paths.get("foo/bar")));
    if (Platform.detect() == Platform.WINDOWS) {
      assertEquals("foo/bar", PathFormatter.pathWithUnixSeparators(Paths.get("foo\\bar")));
    }
  }
}
