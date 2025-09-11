/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util

import com.facebook.kotlin.compilerplugins.kosabi.common.FullTypeQualifier

// https://docs.oracle.com/javase/8/docs/api/java/lang/package-summary.html
object PlainJavaLangTypes : FTQCollection {
  /**
   * Returns the simple names of all types in this collection.
   *
   * @return A collection of simple type names defined in this class
   */
  override fun all(): Collection<String> {
    return listOf(
        "Appendable",
        "AutoCloseable",
        "CharSequence",
        "Cloneable",
        "Comparable",
        "Iterable",
        "Readable",
        "Runnable",
        "Boolean",
        "Byte",
        "Character",
        "Class",
        "ClassLoader",
        "ClassValue",
        "Compiler",
        "Double",
        "Enum",
        "Float",
        "InheritableThreadLocal",
        "Integer",
        "Long",
        "Math",
        "Number",
        "Object",
        "Package",
        "Process",
        "ProcessBuilder",
        "Runtime",
        "RuntimePermission",
        "SecurityManager",
        "Short",
        "StackTraceElement",
        "StrictMath",
        "String",
        "StringBuffer",
        "StringBuilder",
        "System",
        "Thread",
        "ThreadGroup",
        "ThreadLocal",
        "Throwable",
        "Void",
        "Deprecated",
        "FunctionalInterface",
        "Override",
        "SafeVarargs",
        "SuppressWarnings",
        "ArithmeticException",
        "ArrayIndexOutOfBoundsException",
        "ArrayStoreException",
        "ClassCastException",
        "ClassNotFoundException",
        "CloneNotSupportedException",
        "EnumConstantNotPresentException",
        "Exception",
        "IllegalAccessException",
        "IllegalArgumentException",
        "IllegalMonitorStateException",
        "IllegalStateException",
        "IllegalThreadStateException",
        "IndexOutOfBoundsException",
        "InstantiationException",
        "InterruptedException",
        "NegativeArraySizeException",
        "NoSuchFieldException",
        "NoSuchMethodException",
        "NullPointerException",
        "NumberFormatException",
        "ReflectiveOperationException",
        "RuntimeException",
        "SecurityException",
        "StringIndexOutOfBoundsException",
        "TypeNotPresentException",
        "UnsupportedOperationException",
        "AbstractMethodError",
        "AssertionError",
        "BootstrapMethodError",
        "ClassCircularityError",
        "ClassFormatError",
        "Error",
        "ExceptionInInitializerError",
        "IllegalAccessError",
        "IncompatibleClassChangeError",
        "InstantiationError",
        "InternalError",
        "LinkageError",
        "NoClassDefFoundError",
        "NoSuchFieldError",
        "NoSuchMethodError",
        "OutOfMemoryError",
        "StackOverflowError",
        "ThreadDeath",
        "UnknownError",
        "UnsatisfiedLinkError",
        "UnsupportedClassVersionError",
        "VerifyError",
        "VirtualMachineError",
    )
  }
}

object JavaLangTypes {
  val JavaLang_Appendable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Appendable"))
  val JavaLang_AutoCloseable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "AutoCloseable"))
  val JavaLang_CharSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "CharSequence"))
  val JavaLang_Cloneable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Cloneable"))
  val JavaLang_Comparable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Comparable"))
  val JavaLang_Iterable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Iterable"))
  val JavaLang_Readable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Readable"))
  val JavaLang_Runnable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Runnable"))

  val JavaLang_Boolean: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Boolean"))
  val JavaLang_Byte: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Byte"))
  val JavaLang_Character: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Character"))

  val JavaLang_Class: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Class"))
  val JavaLang_ClassLoader: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "ClassLoader"))
  val JavaLang_ClassValue: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "ClassValue"))
  val JavaLang_Compiler: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Compiler"))
  val JavaLang_Double: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Double"))
  val JavaLang_Enum: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Enum"))
  val JavaLang_Float: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Float"))
  val JavaLang_InheritableThreadLocal: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "InheritableThreadLocal"))
  val JavaLang_Integer: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Integer"))
  val JavaLang_Long: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Long"))
  val JavaLang_Math: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Math"))
  val JavaLang_Number: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Number"))
  val JavaLang_Object: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Object"))
  val JavaLang_Package: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Package"))
  val JavaLang_Process: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Process"))
  val JavaLang_ProcessBuilder: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "ProcessBuilder"))
  val JavaLang_Runtime: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Runtime"))
  val JavaLang_RuntimePermission: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "RuntimePermission"))
  val JavaLang_SecurityManager: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "SecurityManager"))
  val JavaLang_Short: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Short"))
  val JavaLang_StackTraceElement: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "StackTraceElement"))
  val JavaLang_StrictMath: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "StrictMath"))
  val JavaLang_String: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "String"))
  val JavaLang_StringBuffer: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "StringBuffer"))
  val JavaLang_StringBuilder: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "StringBuilder"))
  val JavaLang_System: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "System"))
  val JavaLang_Thread: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Thread"))
  val JavaLang_ThreadGroup: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "ThreadGroup"))
  val JavaLang_ThreadLocal: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "ThreadLocal"))
  val JavaLang_Throwable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Throwable"))
  val JavaLang_Void: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Void"))

  val JavaLang_Deprecated: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Deprecated"))
  val JavaLang_FunctionalInterface: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "FunctionalInterface"))
  val JavaLang_Override: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Override"))
  val JavaLang_SafeVarargs: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "SafeVarargs"))
  val JavaLang_SuppressWarnings: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "SuppressWarnings"))

  val JavaLang_ArithmeticException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "ArithmeticException"))
  val JavaLang_ArrayIndexOutOfBoundsException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf("java", "lang", "ArrayIndexOutOfBoundsException")
      )
  val JavaLang_ArrayStoreException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "ArrayStoreException"))
  val JavaLang_ClassCastException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "ClassCastException"))
  val JavaLang_ClassNotFoundException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "ClassNotFoundException"))
  val JavaLang_CloneNotSupportedException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "CloneNotSupportedException"))
  val JavaLang_EnumConstantNotPresentException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf("java", "lang", "EnumConstantNotPresentException")
      )
  val JavaLang_Exception: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Exception"))
  val JavaLang_IllegalAccessException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "IllegalAccessException"))
  val JavaLang_IllegalArgumentException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "IllegalArgumentException"))
  val JavaLang_IllegalMonitorStateException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "IllegalMonitorStateException"))
  val JavaLang_IllegalStateException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "IllegalStateException"))
  val JavaLang_IllegalThreadStateException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "IllegalThreadStateException"))
  val JavaLang_IndexOutOfBoundsException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "IndexOutOfBoundsException"))
  val JavaLang_InstantiationException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "InstantiationException"))
  val JavaLang_InterruptedException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "InterruptedException"))
  val JavaLang_NegativeArraySizeException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "NegativeArraySizeException"))
  val JavaLang_NoSuchFieldException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "NoSuchFieldException"))
  val JavaLang_NoSuchMethodException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "NoSuchMethodException"))
  val JavaLang_NullPointerException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "NullPointerException"))
  val JavaLang_NumberFormatException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "NumberFormatException"))
  val JavaLang_ReflectiveOperationException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "ReflectiveOperationException"))
  val JavaLang_RuntimeException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "RuntimeException"))
  val JavaLang_SecurityException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "SecurityException"))
  val JavaLang_StringIndexOutOfBoundsException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf("java", "lang", "StringIndexOutOfBoundsException")
      )
  val JavaLang_TypeNotPresentException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "TypeNotPresentException"))
  val JavaLang_UnsupportedOperationException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf("java", "lang", "UnsupportedOperationException")
      )
  val JavaLang_AbstractMethodError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "AbstractMethodError"))
  val JavaLang_AssertionError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "AssertionError"))
  val JavaLang_BootstrapMethodError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "BootstrapMethodError"))
  val JavaLang_ClassCircularityError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "ClassCircularityError"))
  val JavaLang_ClassFormatError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "ClassFormatError"))
  val JavaLang_Error: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "Error"))
  val JavaLang_ExceptionInInitializerError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "ExceptionInInitializerError"))
  val JavaLang_IllegalAccessError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "IllegalAccessError"))
  val JavaLang_IncompatibleClassChangeError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "IncompatibleClassChangeError"))
  val JavaLang_InstantiationError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "InstantiationError"))
  val JavaLang_InternalError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "InternalError"))
  val JavaLang_LinkageError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "LinkageError"))
  val JavaLang_NoClassDefFoundError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "NoClassDefFoundError"))
  val JavaLang_NoSuchFieldError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "NoSuchFieldError"))
  val JavaLang_NoSuchMethodError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "NoSuchMethodError"))
  val JavaLang_OutOfMemoryError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "OutOfMemoryError"))
  val JavaLang_StackOverflowError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "StackOverflowError"))
  val JavaLang_ThreadDeath: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "ThreadDeath"))
  val JavaLang_UnknownError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "UnknownError"))
  val JavaLang_UnsatisfiedLinkError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "UnsatisfiedLinkError"))
  val JavaLang_UnsupportedClassVersionError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "UnsupportedClassVersionError"))
  val JavaLang_VerifyError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "VerifyError"))
  val JavaLang_VirtualMachineError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(listOf("java", "lang", "VirtualMachineError"))
}
