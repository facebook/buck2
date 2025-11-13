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

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import com.google.common.base.Joiner;
import com.sun.source.tree.Tree;
import com.sun.source.tree.VariableTree;
import com.sun.source.util.TreePath;
import java.io.IOException;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.TypeParameterElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.util.ElementFilter;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class TreeBackedTreesTest extends CompilerTreeApiParameterizedTest {
  @Test
  public void testIdentifierTreeToName() throws IOException {
    compile("class Foo { String s; }");

    VariableElement stringElement =
        ElementFilter.fieldsIn(elements.getTypeElement("Foo").getEnclosedElements()).get(0);
    VariableTree stringTree = (VariableTree) trees.getTree(stringElement);

    assertNameEquals("String", elements.getName(TreeBackedTrees.treeToName(stringTree.getType())));
  }

  @Test
  public void testMemberSelectTreeToName() throws IOException {
    compile("class Foo { java.lang.String s; }");

    VariableElement stringElement =
        ElementFilter.fieldsIn(elements.getTypeElement("Foo").getEnclosedElements()).get(0);
    VariableTree stringTree = (VariableTree) trees.getTree(stringElement);

    assertNameEquals(
        "java.lang.String", elements.getName(TreeBackedTrees.treeToName(stringTree.getType())));
  }

  @Test
  public void testParameterizedTypeTreeToName() throws IOException {
    compile("class Foo { java.util.List<String> s; }");

    VariableElement listElement =
        ElementFilter.fieldsIn(elements.getTypeElement("Foo").getEnclosedElements()).get(0);
    VariableTree listTree = (VariableTree) trees.getTree(listElement);

    assertNameEquals(
        "java.util.List", elements.getName(TreeBackedTrees.treeToName(listTree.getType())));
  }

  @Test
  public void testAnnotatedTypeTreeToName() throws IOException {
    compile(
        Joiner.on('\n')
            .join(
                "import java.lang.annotation.*;",
                "class Foo { java.util.@Anno List<@Anno String> s; }",
                "@Target(ElementType.TYPE_USE)",
                "@interface Anno { }"));

    VariableElement listElement =
        ElementFilter.fieldsIn(elements.getTypeElement("Foo").getEnclosedElements()).get(0);
    VariableTree listTree = (VariableTree) trees.getTree(listElement);

    assertNameEquals(
        "java.util.List", elements.getName(TreeBackedTrees.treeToName(listTree.getType())));
  }

  @Test
  public void testGetTreeGetPathRoundtripTypeElement() throws IOException {
    compile("class Foo<T, U> { }");

    TypeElement fooElement = elements.getTypeElement("Foo");

    Tree fooTree = trees.getTree(fooElement);
    TreePath fooPath = trees.getPath(fooElement);

    assertSame(fooPath.getLeaf(), fooTree);
    assertSame(fooElement, trees.getElement(fooPath));
  }

  @Test
  public void testGetTreeNullGetPathRoundtripTypeParameterElement() throws IOException {
    compile("class Foo<T, U> { }");

    TypeParameterElement tElement = elements.getTypeElement("Foo").getTypeParameters().get(0);

    TreePath tPath = trees.getPath(tElement);

    assertSame(tElement, trees.getElement(tPath));
  }

  @Test
  public void testNoTreeOrPathForPrecompiledCode() throws IOException {
    compile("class Foo { }");

    TypeElement stringElement = elements.getTypeElement("java.lang.String");

    assertNull(trees.getTree(stringElement));
    assertNull(trees.getPath(stringElement));
  }
}
