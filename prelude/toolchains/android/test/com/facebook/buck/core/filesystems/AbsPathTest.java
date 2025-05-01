/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.core.filesystems;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.junit.Test;

public class AbsPathTest {

  @Test
  public void removePrefix() {
    AbsPath root = AbsPath.of(Paths.get(".").toAbsolutePath());
    assertEquals(RelPath.get(""), root.removePrefix(root));
    assertEquals(RelPath.get("foo"), root.resolve("foo").removePrefix(root));
    assertEquals(RelPath.get("foo/bar"), root.resolve("foo/bar").removePrefix(root));
  }

  @Test
  public void removePrefixIfStartsWith() {
    AbsPath root = AbsPath.of(Paths.get(".").toAbsolutePath());
    assertEquals(RelPath.get(""), root.removePrefixIfStartsWith(root));
    assertEquals(RelPath.get("foo"), root.resolve("foo").removePrefixIfStartsWith(root));
    assertEquals(RelPath.get("foo/bar"), root.resolve("foo/bar").removePrefixIfStartsWith(root));
    assertNull(root.resolve("foo/bar").removePrefixIfStartsWith(root.resolve("baz")));
  }

  @Test
  public void relPath() throws IOException {
    Path relPath = RelPath.get("a/b/c/testFile.txt").getPath();

    IllegalArgumentException exception =
        assertThrows(IllegalArgumentException.class, () -> AbsPath.of(relPath));
    assertThat(exception.getMessage(), startsWith("path must be absolute: "));
  }
}
