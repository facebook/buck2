/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.PathWrapper;
import com.facebook.buck.io.file.MorePaths;
import com.facebook.buck.io.windowsfs.WindowsFS;
import java.io.IOException;
import java.nio.file.Path;

public class CreateSymlinksForTests {
  private static final WindowsFS winFS;

  static {
    winFS = new WindowsFS();
  }

  /**
   * Creates a symlink using platform specific implementations, if there are some.
   *
   * @param symLink symlink to create.
   * @param realFile target of the symlink.
   * @throws IOException
   */
  public static void createSymLink(Path symLink, Path realFile) throws IOException {
    MorePaths.createSymLink(winFS, symLink, realFile);
  }

  /**
   * Creates a symlink using platform specific implementations, if there are some.
   *
   * @param symLink symlink to create.
   * @param realFile target of the symlink.
   * @throws IOException
   */
  public static void createSymLink(AbsPath symLink, PathWrapper realFile) throws IOException {
    createSymLink(symLink.getPath(), realFile.getPath());
  }
}
