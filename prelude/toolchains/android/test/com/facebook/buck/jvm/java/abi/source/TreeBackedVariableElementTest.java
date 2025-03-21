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
import static org.junit.Assert.assertNull;

import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiParameterized;
import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableMap;
import java.io.IOException;
import java.util.stream.Collectors;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.VariableElement;
import org.hamcrest.Matchers;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(CompilerTreeApiParameterized.class)
public class TreeBackedVariableElementTest extends CompilerTreeApiParameterizedTest {
  @Test
  public void testGetConstantValue() throws IOException {
    compile(
        Joiner.on('\n')
            .join("public class Foo {", "  public static final int CONSTANT = 42;", "}"));

    VariableElement variable = findField("CONSTANT", elements.getTypeElement("Foo"));

    assertEquals(42, variable.getConstantValue());
  }

  @Test
  public void testGetConstantValueDelegatedValueMissing() throws IOException {
    withClasspathForJavacOnly(
        ImmutableMap.of(
            "Bar.java",
            Joiner.on('\n')
                .join("public class Bar {", "  public static final int CONSTANT = 42;", "}")));

    compile(
        Joiner.on('\n')
            .join("public class Foo {", "  public static final int CONSTANT = Bar.CONSTANT;", "}"));

    VariableElement variable = findField("CONSTANT", elements.getTypeElement("Foo"));

    if (testingJavac()) {
      assertEquals(42, variable.getConstantValue());
    } else {
      assertNull(variable.getConstantValue());
    }
    assertThat(testCompiler.getDiagnosticMessages(), Matchers.empty());
  }

  @Test
  public void testGetConstantValueComplexValue() throws IOException {
    compile(
        Joiner.on('\n')
            .join(
                "public class Foo {",
                "  private static final int A = 40;",
                "  private static final int B = 2;",
                "  public static final int CONSTANT = A + B;",
                "}"));

    VariableElement variable = findField("CONSTANT", elements.getTypeElement("Foo"));

    assertEquals(42, variable.getConstantValue());
  }

  @Test
  public void testGetConstantValueComplexValueMissing() throws IOException {
    withClasspathForJavacOnly(
        ImmutableMap.of(
            "Bar.java",
            Joiner.on('\n')
                .join("public class Bar {", "  public static final boolean ADD = true;", "}")));

    compile(
        Joiner.on('\n')
            .join(
                "public class Foo {",
                "  private static final boolean ADD = Bar.ADD;",
                "  private static final int A = 40;",
                "  private static final int B = 2;",
                "  public static final int CONSTANT = ADD ? A + B : 42;",
                "}"));

    VariableElement variable = findField("CONSTANT", elements.getTypeElement("Foo"));

    if (testingJavac()) {
      assertEquals(42, variable.getConstantValue());
    } else {
      assertNull(variable.getConstantValue());
    }
    assertThat(testCompiler.getDiagnosticMessages(), Matchers.empty());
  }

  @Test
  public void testGetAnnotationFromField() throws IOException {
    compile(Joiner.on('\n').join("public class Foo {", "  @Deprecated int field;", "}"));

    VariableElement variable = findField("field", elements.getTypeElement("Foo"));
    assertThat(
        variable.getAnnotationMirrors().stream()
            .map(AnnotationMirror::toString)
            .collect(Collectors.toList()),
        Matchers.contains("@java.lang.Deprecated"));
  }

  @Test
  public void testGetAnnotationFromParameter() throws IOException {
    compile(
        Joiner.on('\n')
            .join("public class Foo {", "  public void foo(@Deprecated int parameter) { }", "}"));

    VariableElement variable =
        findParameter("parameter", findMethod("foo", elements.getTypeElement("Foo")));
    assertThat(
        variable.getAnnotationMirrors().stream()
            .map(AnnotationMirror::toString)
            .collect(Collectors.toList()),
        Matchers.contains("@java.lang.Deprecated"));
  }
}
