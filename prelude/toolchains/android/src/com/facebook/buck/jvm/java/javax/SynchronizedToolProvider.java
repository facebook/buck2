/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.javax;

import com.facebook.buck.jvm.java.version.utils.JavaVersionUtils;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;

/**
 * ToolProvider has no synchronization internally, so if we don't synchronize from the outside we
 * could wind up loading the compiler classes multiple times from different class loaders.
 */
public class SynchronizedToolProvider {

  private static Method getPlatformClassLoaderMethod;

  static {
    if (JavaVersionUtils.getMajorVersion() >= 9) {
      try {
        getPlatformClassLoaderMethod = ClassLoader.class.getMethod("getPlatformClassLoader");
      } catch (NoSuchMethodException e) {
        throw new RuntimeException(e);
      }
    }
  }

  public static JavaCompiler getSystemJavaCompiler() {
    JavaCompiler compiler;
    synchronized (ToolProvider.class) {
      compiler = ToolProvider.getSystemJavaCompiler();
    }
    return compiler;
  }

  public static ClassLoader getSystemToolClassLoader() {
    if (JavaVersionUtils.getMajorVersion() >= 9) {
      // The compiler classes are loaded using the platform class loader in Java 9+.
      try {
        return (ClassLoader) getPlatformClassLoaderMethod.invoke(null);
      } catch (IllegalAccessException | InvocationTargetException e) {
        throw new RuntimeException(e);
      }
    }

    ClassLoader classLoader;
    synchronized (ToolProvider.class) {
      classLoader = ToolProvider.getSystemToolClassLoader();
    }
    return classLoader;
  }
}
