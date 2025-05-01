/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.io.file;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.util.environment.Platform;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.junit.Assume;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class MorePathsWindowsTest {

  @Parameters
  public static Object[] data() {
    return new Object[] {
      "C:/some/path", "D:\\other\\path",
    };
  }

  @Parameter public String testPathString;

  @Test
  public void windowsLongPathStringHasCorrectPrefix() {
    Assume.assumeThat(Platform.detect(), is(Platform.WINDOWS));
    String uncPrefix = "\\\\?\\";

    AbsPath path = AbsPath.of(Paths.get(testPathString));
    String longPathString = MorePaths.getWindowsLongPathString(path);
    assertTrue(longPathString.startsWith(uncPrefix));
  }

  @Test
  public void windowsLongPathStringIsAbsolute() {
    Assume.assumeThat(Platform.detect(), is(Platform.WINDOWS));
    String uncPrefix = "\\\\?\\";

    AbsPath path = AbsPath.of(Paths.get(testPathString));
    String longPathString = MorePaths.getWindowsLongPathString(path);
    Path pathComponent = Paths.get(longPathString.substring(uncPrefix.length()));
    assertTrue(pathComponent.isAbsolute());
  }
}
