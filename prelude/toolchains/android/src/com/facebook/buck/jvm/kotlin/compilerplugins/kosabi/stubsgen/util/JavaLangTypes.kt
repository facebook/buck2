/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util

import com.facebook.kotlin.compilerplugins.kosabi.common.FullTypeQualifier

// https://docs.oracle.com/javase/8/docs/api/java/lang/package-summary.html
class JavaLangTypes private constructor() {
  companion object {
    val JavaLang_Appendable: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "Appendable"))
    val JavaLang_AutoCloseable: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "AutoCloseable"))
    val JavaLang_CharSequence: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "CharSequence"))
    val JavaLang_Cloneable: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "Cloneable"))
    val JavaLang_Comparable: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "Comparable"))
    val JavaLang_Iterable: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Iterable"))
    val JavaLang_Readable: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Readable"))
    val JavaLang_Runnable: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Runnable"))

    val JavaLang_Boolean: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Boolean"))
    val JavaLang_Byte: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Byte"))
    val JavaLang_Character: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "Character"))

    val JavaLang_Class: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Class"))
    val JavaLang_ClassLoader: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "ClassLoader"))
    val JavaLang_ClassValue: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "ClassValue"))
    val JavaLang_Compiler: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Compiler"))
    val JavaLang_Double: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Double"))
    val JavaLang_Enum: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Enum"))
    val JavaLang_Float: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Float"))
    val JavaLang_InheritableThreadLocal: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "InheritableThreadLocal"))
    val JavaLang_Integer: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Integer"))
    val JavaLang_Long: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Long"))
    val JavaLang_Math: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Math"))
    val JavaLang_Number: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Number"))
    val JavaLang_Object: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Object"))
    val JavaLang_Package: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Package"))
    val JavaLang_Process: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Process"))
    val JavaLang_ProcessBuilder: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "ProcessBuilder"))
    val JavaLang_Runtime: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Runtime"))
    val JavaLang_RuntimePermission: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "RuntimePermission"))
    val JavaLang_SecurityManager: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "SecurityManager"))
    val JavaLang_Short: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Short"))
    val JavaLang_StackTraceElement: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "StackTraceElement"))
    val JavaLang_StrictMath: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "StrictMath"))
    val JavaLang_String: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "String"))
    val JavaLang_StringBuffer: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "StringBuffer"))
    val JavaLang_StringBuilder: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "StringBuilder"))
    val JavaLang_System: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "System"))
    val JavaLang_Thread: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Thread"))
    val JavaLang_ThreadGroup: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "ThreadGroup"))
    val JavaLang_ThreadLocal: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "ThreadLocal"))
    val JavaLang_Throwable: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "Throwable"))
    val JavaLang_Void: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Void"))

    val JavaLang_Deprecated: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "Deprecated"))
    val JavaLang_FunctionalInterface: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "FunctionalInterface"))
    val JavaLang_Override: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Override"))
    val JavaLang_SafeVarargs: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "SafeVarargs"))
    val JavaLang_SuppressWarnings: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "SuppressWarnings"))

    val JavaLang_ArithmeticException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "ArithmeticException"))
    val JavaLang_ArrayIndexOutOfBoundsException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "ArrayIndexOutOfBoundsException"))
    val JavaLang_ArrayStoreException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "ArrayStoreException"))
    val JavaLang_ClassCastException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "ClassCastException"))
    val JavaLang_ClassNotFoundException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "ClassNotFoundException"))
    val JavaLang_CloneNotSupportedException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "CloneNotSupportedException"))
    val JavaLang_EnumConstantNotPresentException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "EnumConstantNotPresentException"))
    val JavaLang_Exception: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "Exception"))
    val JavaLang_IllegalAccessException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "IllegalAccessException"))
    val JavaLang_IllegalArgumentException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "IllegalArgumentException"))
    val JavaLang_IllegalMonitorStateException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "IllegalMonitorStateException"))
    val JavaLang_IllegalStateException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "IllegalStateException"))
    val JavaLang_IllegalThreadStateException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "IllegalThreadStateException"))
    val JavaLang_IndexOutOfBoundsException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "IndexOutOfBoundsException"))
    val JavaLang_InstantiationException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "InstantiationException"))
    val JavaLang_InterruptedException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "InterruptedException"))
    val JavaLang_NegativeArraySizeException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "NegativeArraySizeException"))
    val JavaLang_NoSuchFieldException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "NoSuchFieldException"))
    val JavaLang_NoSuchMethodException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "NoSuchMethodException"))
    val JavaLang_NullPointerException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "NullPointerException"))
    val JavaLang_NumberFormatException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "NumberFormatException"))
    val JavaLang_ReflectiveOperationException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "ReflectiveOperationException"))
    val JavaLang_RuntimeException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "RuntimeException"))
    val JavaLang_SecurityException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "SecurityException"))
    val JavaLang_StringIndexOutOfBoundsException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "StringIndexOutOfBoundsException"))
    val JavaLang_TypeNotPresentException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "TypeNotPresentException"))
    val JavaLang_UnsupportedOperationException: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "UnsupportedOperationException"))
    val JavaLang_AbstractMethodError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "AbstractMethodError"))
    val JavaLang_AssertionError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "AssertionError"))
    val JavaLang_BootstrapMethodError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "BootstrapMethodError"))
    val JavaLang_ClassCircularityError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "ClassCircularityError"))
    val JavaLang_ClassFormatError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "ClassFormatError"))
    val JavaLang_Error: FullTypeQualifier = FullTypeQualifier(listOf("java", "lang", "Error"))
    val JavaLang_ExceptionInInitializerError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "ExceptionInInitializerError"))
    val JavaLang_IllegalAccessError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "IllegalAccessError"))
    val JavaLang_IncompatibleClassChangeError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "IncompatibleClassChangeError"))
    val JavaLang_InstantiationError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "InstantiationError"))
    val JavaLang_InternalError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "InternalError"))
    val JavaLang_LinkageError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "LinkageError"))
    val JavaLang_NoClassDefFoundError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "NoClassDefFoundError"))
    val JavaLang_NoSuchFieldError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "NoSuchFieldError"))
    val JavaLang_NoSuchMethodError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "NoSuchMethodError"))
    val JavaLang_OutOfMemoryError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "OutOfMemoryError"))
    val JavaLang_StackOverflowError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "StackOverflowError"))
    val JavaLang_ThreadDeath: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "ThreadDeath"))
    val JavaLang_UnknownError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "UnknownError"))
    val JavaLang_UnsatisfiedLinkError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "UnsatisfiedLinkError"))
    val JavaLang_UnsupportedClassVersionError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "UnsupportedClassVersionError"))
    val JavaLang_VerifyError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "VerifyError"))
    val JavaLang_VirtualMachineError: FullTypeQualifier =
        FullTypeQualifier(listOf("java", "lang", "VirtualMachineError"))
  }
}
