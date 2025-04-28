/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.cli.bootstrapper;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/** Utility class for {@link ClassLoaderBootstrapper} */
class ClassLoaderBootstrapperUtils {

  private ClassLoaderBootstrapperUtils() {}

  static void invokeMainMethod(ClassLoader classLoader, String mainClassName, String[] args) {
    Method mainMethod = getMainMethod(classLoader, mainClassName);
    try {
      mainMethod.invoke(null, (Object) args);
    } catch (IllegalAccessException e) {
      throw new IllegalStateException("Can't access main() method.", e);
    } catch (InvocationTargetException e) {
      throw new IllegalStateException("Exception during main() method execution", e.getCause());
    }
  }

  private static Method getMainMethod(ClassLoader classLoader, String mainClassName) {
    Class<?> mainClass = loadClassWithMainFunction(classLoader, mainClassName);
    try {
      return mainClass.getMethod("main", String[].class);
    } catch (NoSuchMethodException e) {
      throw new IllegalStateException("Can't find a main() method in class: " + mainClassName, e);
    }
  }

  private static Class<?> loadClassWithMainFunction(ClassLoader classLoader, String mainClassName) {
    try {
      return classLoader.loadClass(mainClassName);
    } catch (ClassNotFoundException e) {
      throw new IllegalStateException("Can't find class: " + mainClassName, e);
    }
  }
}
