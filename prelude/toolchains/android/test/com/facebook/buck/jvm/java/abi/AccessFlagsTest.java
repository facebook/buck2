/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.abi;

import static org.junit.Assert.assertEquals;

import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiParameterized;
import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiTest;
import com.google.common.base.Joiner;
import java.io.IOException;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.objectweb.asm.Opcodes;

@RunWith(CompilerTreeApiParameterized.class)
public class AccessFlagsTest extends CompilerTreeApiTest {

  @Parameterized.Parameter public String sourceVersion;

  @Parameterized.Parameters(name = "source={0}")
  public static Object[] parameters() {
    return new Object[] {"8", "11"};
  }

  private AccessFlags accessFlags;

  @Before
  public void setUp() {
    testCompiler.setSourceLevel(sourceVersion);
    testCompiler.setTargetLevel(sourceVersion);
    // Ignore "warning: [options] system modules path not set in conjunction with -source"
    testCompiler.setAllowCompilationErrors(true);
  }

  @Test
  public void testPublicFlagOnField() throws IOException {
    testFieldFlags("public", Opcodes.ACC_PUBLIC);
  }

  @Test
  public void testPublicFlagOnMethod() throws IOException {
    testMethodFlags("public", Opcodes.ACC_PUBLIC);
  }

  @Test
  public void testPublicFlagOnClass() throws IOException {
    testClassFlags("public", Opcodes.ACC_PUBLIC);
  }

  @Test
  public void testProtectedFlagOnField() throws IOException {
    testFieldFlags("protected", Opcodes.ACC_PROTECTED);
  }

  @Test
  public void testProtectedFlagOnMethod() throws IOException {
    testMethodFlags("protected", Opcodes.ACC_PROTECTED);
  }

  @Test
  public void testPrivateFlagOnField() throws IOException {
    testFieldFlags("private", Opcodes.ACC_PRIVATE);
  }

  @Test
  public void testPrivateFlagOnMethod() throws IOException {
    testMethodFlags("private", Opcodes.ACC_PRIVATE);
  }

  @Test
  public void testNoFlagForDefaultVisibilityOnField() throws IOException {
    testFieldFlags("", 0);
  }

  @Test
  public void testNoFlagForDefaultVisibilityOnMethod() throws IOException {
    testMethodFlags("", 0);
  }

  @Test
  public void testNoFlagForDefaultVisibilityOnClass() throws IOException {
    testClassFlags("", 0);
  }

  @Test
  public void testNoFlagForInterfaceDefaultMethod() throws IOException {
    compile(Joiner.on('\n').join("interface Foo {", "  default void foo() { }", "}"));

    assertEquals(
        Opcodes.ACC_PUBLIC,
        accessFlags.getAccessFlags(findMethod("foo", elements.getTypeElement("Foo"))));
  }

  @Test
  public void testStaticFlagOnField() throws IOException {
    testFieldFlags("static", Opcodes.ACC_STATIC);
  }

  @Test
  public void testStaticFlagOnMethod() throws IOException {
    testMethodFlags("static", Opcodes.ACC_STATIC);
  }

  @Test
  public void testStaticFlagOnClass() throws IOException {
    testTypeFlags(
        Joiner.on('\n').join("class Foo {", "  static class Inner { }", "}"),
        "Foo.Inner",
        Opcodes.ACC_STATIC | Opcodes.ACC_SUPER);
  }

  @Test
  public void testFinalFlagOnField() throws IOException {
    testFieldFlags("final", Opcodes.ACC_FINAL);
  }

  @Test
  public void testFinalFlagOnMethod() throws IOException {
    testMethodFlags("final", Opcodes.ACC_FINAL);
  }

  @Test
  public void testFinalFlagOnClass() throws IOException {
    testClassFlags("final", Opcodes.ACC_FINAL);
  }

  @Test
  public void testVolatileFlag() throws IOException {
    testFieldFlags("volatile", Opcodes.ACC_VOLATILE);
  }

  @Test
  public void testTransientFlag() throws IOException {
    testFieldFlags("transient", Opcodes.ACC_TRANSIENT);
  }

  @Test
  public void testAbstractFlagOnClass() throws IOException {
    testClassFlags("abstract", Opcodes.ACC_ABSTRACT);
  }

  @Test
  public void testAbstractFlagOnMethod() throws IOException {
    compile(Joiner.on('\n').join("abstract class Foo {", "  abstract void foo();", "}"));
    assertEquals(
        Opcodes.ACC_ABSTRACT,
        accessFlags.getAccessFlags(findMethod("foo", elements.getTypeElement("Foo"))));
  }

  @Test
  public void testSynchronizedFlag() throws IOException {
    testMethodFlags("synchronized", Opcodes.ACC_SYNCHRONIZED);
  }

  @Test
  public void testFpStrictFlag() throws IOException {
    testMethodFlags("strictfp", Opcodes.ACC_STRICT);
  }

  @Test
  public void testNativeFlag() throws IOException {
    compile(Joiner.on('\n').join("class Foo {", "  native void method();", "}"));

    assertEquals(
        Opcodes.ACC_NATIVE,
        accessFlags.getAccessFlags(findMethod("method", elements.getTypeElement("Foo"))));
  }

  @Test
  public void testMultipleFlagsOnMethod() throws IOException {
    testMethodFlags("public static", Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC);
  }

  @Test
  public void testMultipleFlagsOnField() throws IOException {
    testFieldFlags("public static", Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC);
  }

  @Test
  public void testVarArgsFlag() throws IOException {
    compile(Joiner.on('\n').join("class Foo {", "  void method(String... s) { }", "}"));

    assertEquals(
        Opcodes.ACC_VARARGS,
        accessFlags.getAccessFlags(findMethod("method", elements.getTypeElement("Foo"))));
  }

  @Test
  public void testDeprecatedPseudoFlagOnField() throws IOException {
    testFieldFlags("@Deprecated", Opcodes.ACC_DEPRECATED);
  }

  @Test
  public void testDeprecatedPseudoFlagOnMethod() throws IOException {
    testMethodFlags("@Deprecated", Opcodes.ACC_DEPRECATED);
  }

  @Test
  public void testAnnotationTypeFlags() throws IOException {
    testTypeFlags(
        "@java.lang.annotation.Documented @interface Foo { }",
        "Foo",
        Opcodes.ACC_ANNOTATION | Opcodes.ACC_INTERFACE | Opcodes.ACC_ABSTRACT);
  }

  @Test
  public void testInterfaceTypeFlags() throws IOException {
    testTypeFlags("interface Foo { }", "Foo", Opcodes.ACC_INTERFACE | Opcodes.ACC_ABSTRACT);
  }

  @Test
  public void testEnumTypeFlags() throws IOException {
    testTypeFlags(
        "enum Foo { Item }", "Foo", Opcodes.ACC_ENUM | Opcodes.ACC_SUPER | Opcodes.ACC_FINAL);
  }

  @Test
  public void testExplicitEnumAbstractFlag() throws IOException {
    testTypeFlags(
        Joiner.on('\n')
            .join(
                "enum Foo {",
                "  Value {",
                "    int get() { return 3; }",
                "  };",
                "  abstract int get();",
                "}"),
        "Foo",
        Opcodes.ACC_ENUM | Opcodes.ACC_SUPER | Opcodes.ACC_ABSTRACT);
  }

  @Test
  public void testImplicitEnumAbstractFlag() throws IOException {
    testTypeFlags(
        Joiner.on('\n').join("enum Foo implements Runnable {", "  Value;", "}"),
        "Foo",
        Opcodes.ACC_ENUM | Opcodes.ACC_SUPER | Opcodes.ACC_ABSTRACT);
  }

  @Test
  public void testNonAbstractGenericEnumAbstractFlag() throws IOException {
    testTypeFlags(
        Joiner.on('\n')
            .join(
                "enum Foo implements java.util.Comparator<Foo>{",
                "  Value {",
                "    int get() { return 3; }",
                "  };",
                "  public int compare(Foo a, Foo b) { return 0; }",
                "}"),
        "Foo",
        Opcodes.ACC_ENUM | Opcodes.ACC_SUPER);
  }

  @Test
  public void testEnumVarFlags() throws IOException {
    compile("enum Foo { Item }");
    assertEquals(
        Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL | Opcodes.ACC_ENUM,
        accessFlags.getAccessFlags(findField("Item", elements.getTypeElement("Foo"))));
  }

  private void testClassFlags(String modifiers, int expectedFlags) throws IOException {
    testTypeFlags(
        String.format("%s class Foo { }", modifiers), "Foo", expectedFlags | Opcodes.ACC_SUPER);
  }

  private void testTypeFlags(String content, String typeName, int expectedFlags)
      throws IOException {
    compile(content);
    assertNoErrors();
    assertEquals(expectedFlags, accessFlags.getAccessFlags(elements.getTypeElement(typeName)));
  }

  private void testMethodFlags(String modifiers, int expectedFlags) throws IOException {
    compile(
        Joiner.on('\n')
            .join("class Foo {", String.format("  %s void method() { }", modifiers), "}"));

    assertNoErrors();
    assertEquals(
        expectedFlags,
        accessFlags.getAccessFlags(findMethod("method", elements.getTypeElement("Foo"))));
  }

  private void testFieldFlags(String modifiers, int expectedFlags) throws IOException {
    compile(
        Joiner.on('\n').join("class Foo {", String.format("  %s int field = 0;", modifiers), "}"));

    assertNoErrors();
    assertEquals(
        expectedFlags,
        accessFlags.getAccessFlags(findField("field", elements.getTypeElement("Foo"))));
  }

  @Override
  protected void initCompiler(Map<String, String> fileNamesToContents) throws IOException {
    super.initCompiler(fileNamesToContents);
    accessFlags = new AccessFlags(elements);
  }
}
