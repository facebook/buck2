/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi.source;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;

import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiParameterized;
import com.google.common.base.Joiner;
import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.ExecutableElement;
import org.hamcrest.Matchers;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(CompilerTreeApiParameterized.class)
public class TreeBackedAnnotationMirrorTest extends CompilerTreeApiParameterizedTest {
  @Test
  public void testAnnotationMirrorValue() throws IOException {
    compile(
        Joiner.on('\n').join("@FooHelper", "public class Foo {", "}", "@interface FooHelper {}"));

    AnnotationMirror a = elements.getTypeElement("Foo").getAnnotationMirrors().get(0);
    assertSameType(elements.getTypeElement("FooHelper").asType(), a.getAnnotationType());
  }

  @Test
  public void testSingleElementAnnotationMirrorValue() throws IOException {
    compile(
        Joiner.on('\n')
            .join(
                "@FooHelper(42)",
                "public class Foo {",
                "}",
                "@interface FooHelper {",
                "  int value();",
                "}"));

    AnnotationMirror a = elements.getTypeElement("Foo").getAnnotationMirrors().get(0);
    ExecutableElement keyElement = findMethod("value", elements.getTypeElement("FooHelper"));

    assertEquals(1, a.getElementValues().size());
    assertEquals(42, a.getElementValues().get(keyElement).getValue());
  }

  @Test
  public void testSingleElementExplicitAnnotationMirrorValue() throws IOException {
    compile(
        Joiner.on('\n')
            .join(
                "@FooHelper(number = 42)",
                "public class Foo {",
                "}",
                "@interface FooHelper {",
                "  int number();",
                "}"));

    AnnotationMirror a = elements.getTypeElement("Foo").getAnnotationMirrors().get(0);
    ExecutableElement keyElement = findMethod("number", elements.getTypeElement("FooHelper"));

    assertEquals(1, a.getElementValues().size());
    assertEquals(42, a.getElementValues().get(keyElement).getValue());
  }

  @Test
  public void testSingleElementArrayAnnotationMirrorValueWithSingleEntry() throws IOException {
    compile(
        Joiner.on('\n')
            .join(
                "@FooHelper(42)",
                "public class Foo {",
                "}",
                "@interface FooHelper {",
                "  int[] value();",
                "}"));

    AnnotationMirror a = elements.getTypeElement("Foo").getAnnotationMirrors().get(0);
    ExecutableElement keyElement = findMethod("value", elements.getTypeElement("FooHelper"));

    @SuppressWarnings("unchecked")
    List<AnnotationValue> values =
        (List<AnnotationValue>) a.getElementValues().get(keyElement).getValue();
    assertEquals(42, values.get(0).getValue());
    assertEquals(1, a.getElementValues().size());
    assertEquals(1, values.size());
  }

  @Test
  public void testMultiElementAnnotationMirrorValue() throws IOException {
    compile(
        Joiner.on('\n')
            .join(
                "@FooHelper(number=42, string=\"42\", doubleNumber=42.0)",
                "public class Foo {",
                "}",
                "@interface FooHelper {",
                "  double doubleNumber();",
                "  int number();",
                "  String string();",
                "}"));

    AnnotationMirror a = elements.getTypeElement("Foo").getAnnotationMirrors().get(0);

    assertThat(
        a.getElementValues().entrySet().stream()
            .flatMap(
                entry ->
                    Stream.of(
                        entry.getKey().getSimpleName().toString(), entry.getValue().getValue()))
            .collect(Collectors.toList()),
        Matchers.contains("number", 42, "string", "42", "doubleNumber", 42.0));
  }

  @Test
  public void testMethodAnnotatedWithTypeUseAnnotation() throws IOException {
    compile(
        Joiner.on('\n')
            .join(
                "import java.lang.annotation.*;",
                "class Foo { @Override @Anno public String toString() { return \"\"; } }",
                "@Target(ElementType.TYPE_USE)",
                "@interface Anno { }"));
  }

  @Test
  public void testMethodAnnotatedWithTypeAndMethodUseAnnotation() throws IOException {
    compile(
        Joiner.on('\n')
            .join(
                "import java.lang.annotation.*;",
                "class Foo { @Override @Anno public String toString() { return \"\"; } }",
                "@Target({ElementType.TYPE_USE, ElementType.METHOD})",
                "@interface Anno { }"));
  }
}
