/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.abi.source;

import static org.junit.Assert.assertEquals;

import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiParameterized;
import com.google.common.base.Joiner;
import java.io.IOException;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(CompilerTreeApiParameterized.class)
public class StandalonePackageTypeTest extends CompilerTreeApiParameterizedTest {
  @Test
  public void testToString() throws IOException {
    compile(Joiner.on('\n').join("package com.facebook.foo;", "class Foo { }"));

    assertEquals(
        "com.facebook.foo", elements.getPackageElement("com.facebook.foo").asType().toString());
  }
}
