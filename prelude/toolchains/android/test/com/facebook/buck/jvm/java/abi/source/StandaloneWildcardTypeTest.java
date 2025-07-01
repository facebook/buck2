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
import java.io.IOException;
import javax.lang.model.type.WildcardType;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(CompilerTreeApiParameterized.class)
public class StandaloneWildcardTypeTest extends CompilerTreeApiParameterizedTest {
  @Test
  public void testUnboundedToString() throws IOException {
    initCompiler();

    WildcardType wildcard = types.getWildcardType(null, null);

    assertEquals("?", wildcard.toString());
  }

  @Test
  public void testExtendsToString() throws IOException {
    initCompiler();

    WildcardType wildcard =
        types.getWildcardType(elements.getTypeElement("java.lang.CharSequence").asType(), null);

    assertEquals("? extends java.lang.CharSequence", wildcard.toString());
  }

  @Test
  public void testSuperToString() throws IOException {
    initCompiler();

    WildcardType wildcard =
        types.getWildcardType(null, elements.getTypeElement("java.lang.String").asType());

    assertEquals("? super java.lang.String", wildcard.toString());
  }
}
