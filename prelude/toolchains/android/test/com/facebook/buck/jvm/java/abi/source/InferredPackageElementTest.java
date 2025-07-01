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

import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiTest;
import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiTestRunner;
import com.google.common.base.Joiner;
import java.io.IOException;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.DeclaredType;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(CompilerTreeApiTestRunner.class)
public class InferredPackageElementTest extends CompilerTreeApiTest {
  @Before
  public void setUp() {
    testCompiler.useFrontendOnlyJavacTask();
  }

  @Test
  public void testToStringSimpleNamePackage() throws IOException {
    compile(Joiner.on('\n').join("public class Foo extends pkg.Bar.Baz {", "}"));

    DeclaredType superclass = (DeclaredType) elements.getTypeElement("Foo").getSuperclass();
    TypeElement superclassElement = (TypeElement) superclass.asElement();
    TypeElement superclassEnclosingTypeElement =
        (TypeElement) superclassElement.getEnclosingElement();
    PackageElement element =
        (InferredPackageElement) superclassEnclosingTypeElement.getEnclosingElement();

    assertEquals("pkg", element.toString());
  }

  @Test
  public void testToStringQualifiedNamePackage() throws IOException {
    compile(Joiner.on('\n').join("public class Foo extends com.example.foo.Bar.Baz {", "}"));

    DeclaredType superclass = (DeclaredType) elements.getTypeElement("Foo").getSuperclass();
    TypeElement superclassElement = (TypeElement) superclass.asElement();
    TypeElement superclassEnclosingTypeElement =
        (TypeElement) superclassElement.getEnclosingElement();
    PackageElement element =
        (InferredPackageElement) superclassEnclosingTypeElement.getEnclosingElement();

    assertEquals("com.example.foo", element.toString());
  }
}
