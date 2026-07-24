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
