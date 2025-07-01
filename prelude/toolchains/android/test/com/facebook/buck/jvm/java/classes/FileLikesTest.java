/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.classes;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.InputStream;
import java.nio.file.Path;
import java.util.Objects;
import org.junit.Test;

public class FileLikesTest {

  @Test
  public void testIsClassFile() {
    assertTrue(FileLikes.isClassFile(new FakeFileLike("com/example/Bar.class")));
    assertFalse(FileLikes.isClassFile(new FakeFileLike("com/example/Bar.txt")));
  }

  @Test(expected = NullPointerException.class)
  public void testIsClassFileRejectsNull() {
    assertTrue(FileLikes.isClassFile(null));
  }

  @Test
  public void testGetFileNameWithoutClassSuffix() {
    assertEquals(
        "com/example/Bar",
        FileLikes.getFileNameWithoutClassSuffix(new FakeFileLike("com/example/Bar.class")));
    assertEquals(
        "com/example/Foo$1",
        FileLikes.getFileNameWithoutClassSuffix(new FakeFileLike("com/example/Foo$1.class")));
  }

  private static class FakeFileLike implements FileLike {

    private final String relativePath;

    private FakeFileLike(String relativePath) {
      this.relativePath = Objects.requireNonNull(relativePath);
    }

    @Override
    public String getRelativePath() {
      return relativePath;
    }

    @Override
    public Path getContainer() {
      throw new UnsupportedOperationException();
    }

    @Override
    public long getSize() {
      throw new UnsupportedOperationException();
    }

    @Override
    public InputStream getInput() {
      throw new UnsupportedOperationException();
    }
  }
}
