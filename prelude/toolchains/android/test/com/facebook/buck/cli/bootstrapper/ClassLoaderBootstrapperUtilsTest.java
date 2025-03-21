/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.cli.bootstrapper;

import org.hamcrest.Matchers;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class ClassLoaderBootstrapperUtilsTest {

  @Rule public ExpectedException expectedException = ExpectedException.none();

  @Test
  public void runExternalClassWithException() {
    expectedException.expect(IllegalStateException.class);
    expectedException.expectMessage("Exception during main() method execution");
    expectedException.expectCause(Matchers.isA(TestMain.HumanReadableException.class));

    ClassLoaderBootstrapperUtils.invokeMainMethod(
        Thread.currentThread().getContextClassLoader(),
        TestMain.class.getCanonicalName(),
        new String[] {Boolean.toString(true)});
  }

  @Test
  public void runExternalClassWithoutException() {
    ClassLoaderBootstrapperUtils.invokeMainMethod(
        Thread.currentThread().getContextClassLoader(),
        TestMain.class.getCanonicalName(),
        new String[] {Boolean.toString(false)});
  }
}
