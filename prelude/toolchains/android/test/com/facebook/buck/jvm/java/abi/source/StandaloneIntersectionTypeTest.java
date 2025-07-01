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
import static org.junit.Assert.assertSame;

import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiParameterized;
import java.io.IOException;
import java.util.List;
import javax.lang.model.type.IntersectionType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(CompilerTreeApiParameterized.class)
public class StandaloneIntersectionTypeTest extends CompilerTreeApiParameterizedTest {
  @Test
  public void testGetKind() throws IOException {
    compile("class Foo<T extends java.lang.Runnable & java.lang.CharSequence> { }");

    TypeMirror intersectionType = getTypeParameterUpperBound("Foo", 0);

    assertSame(TypeKind.INTERSECTION, intersectionType.getKind());
  }

  @Test
  public void testGetBounds() throws IOException {
    compile("class Foo<T extends java.lang.Runnable & java.lang.CharSequence> { }");

    TypeMirror runnableType = elements.getTypeElement("java.lang.Runnable").asType();
    TypeMirror charSequenceType = elements.getTypeElement("java.lang.CharSequence").asType();

    IntersectionType intersectionType = (IntersectionType) getTypeParameterUpperBound("Foo", 0);
    List<? extends TypeMirror> bounds = intersectionType.getBounds();

    assertSame(2, bounds.size());
    assertSameType(runnableType, bounds.get(0));
    assertSameType(charSequenceType, bounds.get(1));
  }

  @Test
  public void testToString() throws IOException {
    compile("class Foo<T extends java.lang.Runnable & java.lang.CharSequence> { }");

    TypeMirror intersectionType = getTypeParameterUpperBound("Foo", 0);

    assertEquals(
        "java.lang.Object&java.lang.Runnable&java.lang.CharSequence", intersectionType.toString());
  }
}
