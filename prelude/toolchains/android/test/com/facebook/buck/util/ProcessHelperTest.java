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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

public class ProcessHelperTest {

  private ProcessHelper processHelper;

  @Before
  public void setUp() {
    processHelper = ProcessHelper.getInstance();
  }

  @Test
  public void testHasNuProcessFinished() {
    FakeNuProcess nuProcess = new FakeNuProcess(1234);
    assertFalse(processHelper.hasProcessFinished(nuProcess));
    nuProcess.finish(0);
    assertTrue(processHelper.hasProcessFinished(nuProcess));
  }

  @Test
  public void testHasJavaProcessFinished() throws Exception {
    Process process = new FakeProcess(42);
    assertFalse(processHelper.hasProcessFinished(process));
    process.waitFor();
    assertTrue(processHelper.hasProcessFinished(process));
  }
}
