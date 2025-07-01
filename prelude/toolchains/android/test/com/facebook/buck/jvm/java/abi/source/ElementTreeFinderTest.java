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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiTest;
import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiTestRunner;
import com.google.common.base.Joiner;
import com.sun.source.tree.CompilationUnitTree;
import java.io.IOException;
import javax.lang.model.element.TypeElement;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(CompilerTreeApiTestRunner.class)
public class ElementTreeFinderTest extends CompilerTreeApiTest {
  private static final String SOURCE_CODE =
      Joiner.on('\n')
          .join(
              "class AClass<ClassTypeParam> {",
              "  public <MethodTypeParam> void method(int methodParameter) {",
              "  }",
              "  private int field;",
              "}");

  private CompilationUnitTree compilationUnit;
  private ElementTreeFinder finder;

  @Before
  public void setUp() throws IOException {
    compile(SOURCE_CODE);
    compilationUnit = getCompilationUnit();
    finder = ElementTreeFinder.forCompilationUnit(compilationUnit, trees);
  }

  @Test
  public void testFindsTreeForClass() {
    assertThat(
        finder.getTree(elements.getTypeElement("AClass")),
        is(TreeFinder.findTreeNamed(compilationUnit, "AClass")));
  }

  @Test
  public void testFindsTreeForClassTypeParameter() {
    assertThat(
        finder.getTree(findTypeParameter("ClassTypeParam", elements.getTypeElement("AClass"))),
        is(TreeFinder.findTreeNamed(compilationUnit, "ClassTypeParam")));
  }

  @Test
  public void testFindsTreeForMethod() {
    assertThat(
        finder.getTree(findMethod("method", elements.getTypeElement("AClass"))),
        is(TreeFinder.findTreeNamed(compilationUnit, "method")));
  }

  @Test
  public void testFindsTreeForMethodParameter() {
    assertThat(
        finder.getTree(
            findParameter(
                "methodParameter", findMethod("method", elements.getTypeElement("AClass")))),
        is(TreeFinder.findTreeNamed(compilationUnit, "methodParameter")));
  }

  @Test
  public void testFindsTreeForMethodTypeParameter() {
    assertThat(
        finder.getTree(
            findTypeParameter(
                "MethodTypeParam", findMethod("method", elements.getTypeElement("AClass")))),
        is(TreeFinder.findTreeNamed(compilationUnit, "MethodTypeParam")));
  }

  @Test
  public void testFindsTreeForField() {
    assertThat(
        finder.getTree(findField("field", elements.getTypeElement("AClass"))),
        is(TreeFinder.findTreeNamed(compilationUnit, "field")));
  }

  private CompilationUnitTree getCompilationUnit() {
    TypeElement foo = elements.getTypeElement("AClass");
    return trees.getPath(foo).getCompilationUnit();
  }
}
