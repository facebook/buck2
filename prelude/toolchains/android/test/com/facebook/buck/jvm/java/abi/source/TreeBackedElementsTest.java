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
import static org.junit.Assert.assertTrue;

import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiParameterized;
import com.google.common.base.Joiner;
import java.io.IOException;
import java.util.Map;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(CompilerTreeApiParameterized.class)
public class TreeBackedElementsTest extends CompilerTreeApiParameterizedTest {
  @Test
  public void testGetBinaryNameTopLevelClass() throws IOException {
    compile(Joiner.on('\n').join("package com.facebook.foo;", "class Foo { }"));

    assertEquals(
        "com.facebook.foo.Foo",
        elements.getBinaryName(elements.getTypeElement("com.facebook.foo.Foo")).toString());
  }

  @Test
  public void testGetBinaryNameInnerClass() throws IOException {
    compile(
        Joiner.on('\n').join("package com.facebook.foo;", "class Foo {", "  class Inner {}", "}"));

    assertEquals(
        "com.facebook.foo.Foo$Inner",
        elements.getBinaryName(elements.getTypeElement("com.facebook.foo.Foo.Inner")).toString());
  }

  @Test
  public void testGetDocComment() throws IOException {
    compile(
        Joiner.on('\n')
            .join("package com.facebook.foo;", "/** I am a doc comment. */", "class Foo { }"));

    assertEquals(
        "I am a doc comment. ",
        elements.getDocComment(elements.getTypeElement("com.facebook.foo.Foo")));
  }

  @Test
  public void testIsDeprecated() throws IOException {
    compile(Joiner.on('\n').join("package com.facebook.foo;", "@Deprecated", "class Foo { }"));

    assertTrue(elements.isDeprecated(elements.getTypeElement("com.facebook.foo.Foo")));
  }

  @Test
  public void testGetPackageOf() throws IOException {
    compile(Joiner.on('\n').join("package com.facebook.foo;", "@Deprecated", "class Foo { }"));

    assertSame(
        elements.getPackageElement("com.facebook.foo"),
        elements.getPackageOf(elements.getTypeElement("com.facebook.foo.Foo")));
  }

  @Test
  public void testGetElementValuesWithDefaults() throws IOException {
    compile(
        Joiner.on('\n')
            .join(
                "package com.facebook.foo;",
                "@Anno(a=4)",
                "class Foo { }",
                "@interface Anno {",
                "  int a() default 1;",
                "  int b() default 2;",
                "}"));

    TypeElement fooType = elements.getTypeElement("com.facebook.foo.Foo");
    TypeElement annotationType = elements.getTypeElement("com.facebook.foo.Anno");

    ExecutableElement aParam = findMethod("a", annotationType);
    ExecutableElement bParam = findMethod("b", annotationType);

    AnnotationMirror annotation = fooType.getAnnotationMirrors().get(0);
    Map<? extends ExecutableElement, ? extends AnnotationValue> elementValuesWithDefaults =
        elements.getElementValuesWithDefaults(annotation);

    assertEquals(4, elementValuesWithDefaults.get(aParam).getValue());
    assertEquals(2, elementValuesWithDefaults.get(bParam).getValue());
    assertEquals(2, elementValuesWithDefaults.size());
  }
}
