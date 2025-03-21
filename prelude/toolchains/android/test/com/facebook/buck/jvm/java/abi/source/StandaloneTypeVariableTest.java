/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;

import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiParameterized;
import java.io.IOException;
import java.util.List;
import javax.lang.model.element.TypeParameterElement;
import javax.lang.model.type.IntersectionType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.type.TypeVariable;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(CompilerTreeApiParameterized.class)
public class StandaloneTypeVariableTest extends CompilerTreeApiParameterizedTest {
  @Test
  public void testGetKind() throws IOException {
    compile("class Foo<T> { }");

    TypeVariable tVar =
        (TypeVariable) elements.getTypeElement("Foo").getTypeParameters().get(0).asType();

    assertSame(TypeKind.TYPEVAR, tVar.getKind());
  }

  @Test
  public void testAsElement() throws IOException {
    compile("class Foo<T> { }");

    TypeParameterElement tElement = elements.getTypeElement("Foo").getTypeParameters().get(0);
    TypeVariable tVar = (TypeVariable) tElement.asType();

    assertSame(tElement, tVar.asElement());
  }

  @Test
  public void testGetUpperBoundUnbounded() throws IOException {
    compile("class Foo<T> { }");

    TypeMirror objectType = elements.getTypeElement("java.lang.Object").asType();

    TypeVariable tVar =
        (TypeVariable) elements.getTypeElement("Foo").getTypeParameters().get(0).asType();

    assertSameType(objectType, tVar.getUpperBound());
  }

  @Test
  public void testGetLowerBoundUnbounded() throws IOException {
    compile("class Foo<T> { }");

    TypeVariable tVar =
        (TypeVariable) elements.getTypeElement("Foo").getTypeParameters().get(0).asType();

    assertSameType(types.getNullType(), tVar.getLowerBound());
  }

  @Test
  public void testToStringUnbounded() throws IOException {
    compile("class Foo<T> { }");

    TypeVariable typeVariable =
        (TypeVariable) elements.getTypeElement("Foo").getTypeParameters().get(0).asType();

    assertEquals("T", typeVariable.toString());
  }

  @Test
  public void testGetUpperBoundMultipleBounds() throws IOException {
    compile("class Foo<T extends java.lang.CharSequence & java.lang.Runnable> { }");

    TypeMirror charSequenceType = elements.getTypeElement("java.lang.CharSequence").asType();
    TypeMirror runnableType = elements.getTypeElement("java.lang.Runnable").asType();

    TypeVariable tVar =
        (TypeVariable) elements.getTypeElement("Foo").getTypeParameters().get(0).asType();

    IntersectionType upperBound = (IntersectionType) tVar.getUpperBound();

    List<? extends TypeMirror> bounds = upperBound.getBounds();
    assertSame(2, bounds.size());
    assertSameType(charSequenceType, bounds.get(0));
    assertSameType(runnableType, bounds.get(1));
  }

  @Test
  public void testGetLowerBoundMultipleBounds() throws IOException {
    compile("class Foo<T extends java.lang.Runnable & java.lang.CharSequence> { }");

    TypeVariable tVar =
        (TypeVariable) elements.getTypeElement("Foo").getTypeParameters().get(0).asType();

    assertSameType(types.getNullType(), tVar.getLowerBound());
  }

  @Test
  public void testToStringBounded() throws IOException {
    compile("class Foo<T extends java.lang.CharSequence> { }");

    TypeVariable typeVariable =
        (TypeVariable) elements.getTypeElement("Foo").getTypeParameters().get(0).asType();

    assertEquals("T", typeVariable.toString());
  }

  @Test
  public void testToStringMultipleBounds() throws IOException {
    compile("class Foo<T extends java.lang.CharSequence & java.lang.Runnable> { }");

    TypeVariable typeVariable =
        (TypeVariable) elements.getTypeElement("Foo").getTypeParameters().get(0).asType();

    assertEquals("T", typeVariable.toString());
  }
}
