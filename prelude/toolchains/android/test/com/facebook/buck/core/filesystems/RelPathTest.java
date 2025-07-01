/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.core.filesystems;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertThrows;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.junit.Test;

public class RelPathTest {

  @Test
  public void ofPath() {
    for (String path : new String[] {"", "a", "a/bbb", "././aa/.."}) {
      assertThat(RelPath.get(path), equalTo(RelPath.of(Paths.get(path))));
    }
  }

  @Test
  public void absPath() throws IOException {
    Path absPath = Paths.get(".").normalize().toAbsolutePath();

    IllegalArgumentException exception =
        assertThrows(IllegalArgumentException.class, () -> RelPath.of(absPath));
    assertThat(exception.getMessage(), startsWith("path must be relative: "));
  }
}
