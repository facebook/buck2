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
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiParameterized;
import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableMap;
import java.io.IOException;
import java.util.Set;
import java.util.stream.Collectors;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.Name;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.SimpleElementVisitor8;
import org.hamcrest.Matchers;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(CompilerTreeApiParameterized.class)
public class TreeBackedPackageElementTest extends CompilerTreeApiParameterizedTest {
  @Test
  public void testToStringUnnamedPackage() throws IOException {
    compile("class Foo { }");

    assertEquals("unnamed package", elements.getPackageElement("").toString());
  }

  @Test
  public void testToStringSimpleNamedPackage() throws IOException {
    compile(Joiner.on('\n').join("package foo;", "class Foo { }"));

    assertEquals("foo", elements.getPackageElement("foo").toString());
  }

  @Test
  public void testToStringQualifiedNamedPackage() throws IOException {
    initCompiler();

    assertEquals("java.lang", elements.getPackageElement("java.lang").toString());
  }

  @Test
  public void testNoElementForEmptyPackages() throws IOException {
    compile("package foo;");

    assertNull(elements.getPackageElement("foo"));
  }

  @Test
  public void testNoElementForPackagesContainingOnlyPackages() throws IOException {
    initCompiler();

    assertNull(elements.getPackageElement("java"));
  }

  @Test
  public void testGetKind() throws IOException {
    initCompiler();

    assertSame(ElementKind.PACKAGE, elements.getPackageElement("java.lang").getKind());
  }

  @Test
  public void testAccept() throws IOException {
    initCompiler();

    PackageElement expectedPackage = elements.getPackageElement("java.lang");
    Object expectedResult = new Object();
    Object actualResult =
        expectedPackage.accept(
            new SimpleElementVisitor8<Object, Object>() {
              @Override
              protected Object defaultAction(Element e, Object o) {
                return null;
              }

              @Override
              public Object visitPackage(PackageElement actualPackage, Object o) {
                assertSame(expectedPackage, actualPackage);
                return o;
              }
            },
            expectedResult);

    assertSame(expectedResult, actualResult);
  }

  @Test
  public void testUnnamedPackageHasEmptyNames() throws IOException {
    compile("class Foo { }");

    PackageElement unnamedPackage = elements.getPackageElement("");
    Name emptyName = elements.getName("");

    assertTrue(unnamedPackage.isUnnamed());
    assertSame(emptyName, unnamedPackage.getSimpleName());
    assertSame(emptyName, unnamedPackage.getQualifiedName());
  }

  @Test
  public void testUnnamedPackageCanContainStuff() throws IOException {
    compile("class Foo { }");

    PackageElement unnamedPackage = elements.getPackageElement("");
    TypeElement fooElement = elements.getTypeElement("Foo");

    assertPackageContains(unnamedPackage, fooElement);
  }

  @Test
  public void testCanExtendPackageFromDependencies() throws IOException {
    withClasspath(
        ImmutableMap.of(
            "com/example/buck/List.java",
            Joiner.on('\n').join("package com.example.buck;", "public class List { }")));

    compile(Joiner.on('\n').join("package com.example.buck;", "class Foo { }"));

    PackageElement buckPackage = elements.getPackageElement("com.example.buck");
    TypeElement listType = elements.getTypeElement("com.example.buck.List");
    TypeElement fooType = elements.getTypeElement("com.example.buck.Foo");

    assertPackageContains(buckPackage, listType);
    assertPackageContains(buckPackage, fooType);
  }

  @Test
  public void testGetAnnotationMirrorsNoAnnotations() throws IOException {
    compile(Joiner.on('\n').join("package foo;", "class Foo { }"));

    PackageElement fooPackage = elements.getPackageElement("foo");
    assertThat(fooPackage.getAnnotationMirrors(), Matchers.empty());
  }

  @Test
  public void testGetAnnotationMirrorsWithAnnotations() throws IOException {
    compile(
        ImmutableMap.of("package-info.java", Joiner.on('\n').join("@Deprecated", "package foo;")));

    PackageElement fooPackage = elements.getPackageElement("foo");
    assertThat(
        fooPackage.getAnnotationMirrors().stream()
            .map(it -> it.getAnnotationType().asElement().getSimpleName().toString())
            .collect(Collectors.toList()),
        Matchers.contains("Deprecated"));
  }

  private void assertPackageContains(PackageElement packageElement, TypeElement typeElement) {
    // We compare strings because there's some funkiness around built-in types, and they can
    // end up having multiple Elements for them depending on how you arrive at them.
    Set<String> enclosedElements =
        packageElement.getEnclosedElements().stream()
            .map(Object::toString)
            .collect(Collectors.toSet());

    assertTrue(enclosedElements.contains(typeElement.toString()));
  }
}
