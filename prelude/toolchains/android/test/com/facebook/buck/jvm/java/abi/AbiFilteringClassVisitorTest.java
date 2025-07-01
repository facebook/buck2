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

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import org.junit.Before;
import org.junit.Test;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.Opcodes;

public class AbiFilteringClassVisitorTest {
  private ClassVisitor mockVisitor;
  private AbiFilteringClassVisitor filteringVisitor;

  @Before
  public void setUp() {
    mockVisitor = mock(ClassVisitor.class);
    filteringVisitor =
        new AbiFilteringClassVisitor(mockVisitor, ImmutableList.of(), ImmutableSet.of(), false);
  }

  @Test
  public void testExcludesPrivateFields() {
    testExcludesFieldWithAccess(Opcodes.ACC_PRIVATE);
  }

  @Test
  public void testExcludesPrivateStaticFields() {
    testExcludesFieldWithAccess(Opcodes.ACC_PRIVATE | Opcodes.ACC_STATIC);
  }

  @Test
  public void testExcludesSyntheticFields() {
    testExcludesFieldWithAccess(Opcodes.ACC_PUBLIC | Opcodes.ACC_SYNTHETIC);
  }

  @Test
  public void testIncludesPackageFields() {
    testIncludesFieldWithAccess(0);
  }

  @Test
  public void testIncludesPackageStaticFields() {
    testIncludesFieldWithAccess(Opcodes.ACC_STATIC);
  }

  @Test
  public void testIncludesPublicFields() {
    testIncludesFieldWithAccess(Opcodes.ACC_PUBLIC);
  }

  @Test
  public void testIncludesProtectedFields() {
    testIncludesFieldWithAccess(Opcodes.ACC_PROTECTED);
  }

  @Test
  public void testNotConfusedByOtherFieldAccessFlagsIncluding() {
    testIncludesFieldWithAccess(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_VOLATILE);
  }

  @Test
  public void testNotConfusedByOtherFieldAccessFlagsExcluding() {
    testExcludesFieldWithAccess(Opcodes.ACC_PRIVATE | Opcodes.ACC_STATIC | Opcodes.ACC_VOLATILE);
  }

  @Test
  public void testExcludesPrivateMethods() {
    testExcludesMethodWithAccess(Opcodes.ACC_PRIVATE);
  }

  @Test
  public void testIncludesPrivateMethodsWhenRetained() {
    filteringVisitor =
        new AbiFilteringClassVisitor(
            mockVisitor, ImmutableList.of("foo"), ImmutableSet.of(), false);
    testIncludesMethodWithAccess(Opcodes.ACC_PRIVATE);
  }

  @Test
  public void testIncludesPackageMethods() {
    testIncludesMethodWithAccess(Opcodes.ACC_PUBLIC);
  }

  @Test
  public void testIncludesProtectedMethods() {
    testIncludesMethodWithAccess(Opcodes.ACC_PUBLIC);
  }

  @Test
  public void testIncludesPublicMethods() {
    testIncludesMethodWithAccess(Opcodes.ACC_PUBLIC);
  }

  @Test
  public void testExcludesSyntheticMethods() {
    testExcludesMethodWithAccess(Opcodes.ACC_PUBLIC | Opcodes.ACC_SYNTHETIC);
  }

  @Test
  public void testNotConfusedByOtherMethodAccessFlagsIncluding() {
    testIncludesMethodWithAccess(
        Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT | Opcodes.ACC_SYNCHRONIZED);
  }

  @Test
  public void testNotConfusedByOtherMethodAccessFlagsExcluding() {
    testExcludesMethodWithAccess(
        Opcodes.ACC_PRIVATE | Opcodes.ACC_ABSTRACT | Opcodes.ACC_SYNCHRONIZED);
  }

  @Test
  public void testExcludesStaticInitializers() {
    testExcludesMethodWithAccess(Opcodes.ACC_STATIC, "<clinit>");
  }

  @Test
  public void testAlwaysVisitsClassNode() {
    visitClass(filteringVisitor, "Foo");
    verifyVisitClass(mockVisitor, "Foo");
    verifyNoMoreInteractions(mockVisitor);
  }

  @Test
  public void testIncludesInnerClassEntryForClassItself() {
    visitClass(filteringVisitor, "Foo$Inner");
    filteringVisitor.visitInnerClass("Foo$Inner", "Foo", "Inner", Opcodes.ACC_PUBLIC);

    verifyVisitClass(mockVisitor, "Foo$Inner");
    verify(mockVisitor).visitInnerClass("Foo$Inner", "Foo", "Inner", Opcodes.ACC_PUBLIC);
    verifyNoMoreInteractions(mockVisitor);
  }

  @Test
  public void testIncludesInnerClassEntryForInnerClass() {
    visitClass(filteringVisitor, "Foo");
    filteringVisitor.visitInnerClass("Foo$Inner", "Foo", "Inner", Opcodes.ACC_PUBLIC);

    verifyVisitClass(mockVisitor, "Foo");
    verify(mockVisitor).visitInnerClass("Foo$Inner", "Foo", "Inner", Opcodes.ACC_PUBLIC);
    verifyNoMoreInteractions(mockVisitor);
  }

  @Test
  public void testIncludesInnerClassEntryForReferencedOtherClassInnerClass() {
    filteringVisitor =
        new AbiFilteringClassVisitor(
            mockVisitor, ImmutableList.of(), ImmutableSet.of("Bar$Inner"), false);

    visitClass(filteringVisitor, "Foo");
    filteringVisitor.visitInnerClass("Bar$Inner", "Bar", "Inner", Opcodes.ACC_PUBLIC);

    verifyVisitClass(mockVisitor, "Foo");
    verify(mockVisitor).visitInnerClass("Bar$Inner", "Bar", "Inner", Opcodes.ACC_PUBLIC);
    verifyNoMoreInteractions(mockVisitor);
  }

  @Test
  public void testExcludesInnerClassEntryForUnreferencedOtherClassInnerClass() {
    visitClass(filteringVisitor, "Foo");
    filteringVisitor.visitInnerClass("Bar$Inner", "Bar", "Inner", Opcodes.ACC_PUBLIC);

    verifyVisitClass(mockVisitor, "Foo");
    verifyNoMoreInteractions(mockVisitor);
  }

  @Test
  public void testIncludesPrivateInnerClassesForNow() {
    visitClass(filteringVisitor, "Foo");
    filteringVisitor.visitInnerClass("Foo$Inner", "Foo", "Inner", Opcodes.ACC_PRIVATE);

    verifyVisitClass(mockVisitor, "Foo");
    verify(mockVisitor).visitInnerClass("Foo$Inner", "Foo", "Inner", Opcodes.ACC_PRIVATE);
    verifyNoMoreInteractions(mockVisitor);
  }

  @Test
  public void testExcludesSyntheticInnerClasses() {
    visitClass(filteringVisitor, "Foo");
    filteringVisitor.visitInnerClass(
        "Foo$Inner", "Foo", "Inner", Opcodes.ACC_PUBLIC | Opcodes.ACC_SYNTHETIC);

    verifyVisitClass(mockVisitor, "Foo");
    verifyNoMoreInteractions(mockVisitor);
  }

  @Test
  public void testExcludesAnonymousInnerClasses() {
    visitClass(filteringVisitor, "Foo");
    filteringVisitor.visitInnerClass("Foo$1", null, null, 0);

    verifyVisitClass(mockVisitor, "Foo");
    verifyNoMoreInteractions(mockVisitor);
  }

  @Test
  public void testExcludesLocalClasses() {
    visitClass(filteringVisitor, "Foo");
    filteringVisitor.visitInnerClass("Foo$1Bar", null, "Bar", 0);

    verifyVisitClass(mockVisitor, "Foo");
    verifyNoMoreInteractions(mockVisitor);
  }

  private static void visitClass(ClassVisitor cv, String name) {
    cv.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, name, null, "java/lang/Object", null);
  }

  private static void verifyVisitClass(ClassVisitor cv, String name) {
    verify(cv).visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, name, null, "java/lang/Object", null);
  }

  private void testExcludesFieldWithAccess(int access) {
    testFieldWithAccess(access, false);
  }

  private void testIncludesFieldWithAccess(int access) {
    testFieldWithAccess(access, true);
  }

  private void testFieldWithAccess(int access, boolean shouldInclude) {
    filteringVisitor.visitField(access, "Foo", "I", null, null);
    if (shouldInclude) {
      verify(mockVisitor).visitField(access, "Foo", "I", null, null);
    }
  }

  private void testExcludesMethodWithAccess(int access) {
    testExcludesMethodWithAccess(access, "foo");
  }

  private void testIncludesMethodWithAccess(int access) {
    testIncludesMethodWithAccess(access, "foo");
  }

  private void testExcludesMethodWithAccess(int access, String name) {
    testMethodWithAccess(access, name, false);
  }

  private void testIncludesMethodWithAccess(int access, String name) {
    testMethodWithAccess(access, name, true);
  }

  private void testMethodWithAccess(int access, String name, boolean shouldInclude) {
    filteringVisitor.visitMethod(access, name, "()V", null, null);
    if (shouldInclude) {
      verify(mockVisitor).visitMethod(access, name, "()V", null, null);
    }
  }
}
