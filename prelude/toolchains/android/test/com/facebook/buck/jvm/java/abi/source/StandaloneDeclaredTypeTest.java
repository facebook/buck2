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
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeMirror;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(CompilerTreeApiParameterized.class)
public class StandaloneDeclaredTypeTest extends CompilerTreeApiParameterizedTest {
  @Test
  public void testToStringNoGenerics() throws IOException {
    compile(Joiner.on('\n').join("package com.facebook.foo;", "class Foo { }"));

    DeclaredType fooType = (DeclaredType) elements.getTypeElement("com.facebook.foo.Foo").asType();

    assertEquals("com.facebook.foo.Foo", fooType.toString());
  }

  @Test
  public void testToStringWithGenerics() throws IOException {
    initCompiler();

    TypeElement mapElement = elements.getTypeElement("java.util.Map");
    TypeMirror stringType = elements.getTypeElement("java.lang.String").asType();
    TypeMirror integerType = elements.getTypeElement("java.lang.Integer").asType();
    DeclaredType mapStringIntType = types.getDeclaredType(mapElement, stringType, integerType);

    assertEquals("java.util.Map<java.lang.String,java.lang.Integer>", mapStringIntType.toString());
  }
}
