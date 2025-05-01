/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.testutil.compiler;

import com.facebook.buck.jvm.java.javax.SynchronizedToolProvider;
import com.facebook.buck.jvm.java.version.utils.JavaVersionUtils;
import com.facebook.buck.util.MoreSuppliers;
import com.google.common.base.Preconditions;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.function.Supplier;
import javax.tools.ToolProvider;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.model.InitializationError;

/**
 * Test runner that enables tests to work with the Compiler Tree API implementation corresponding to
 * the compiler returned by {@link ToolProvider#getSystemJavaCompiler()}. In Java 8 and earlier,
 * these are public APIs that are not provided in rt.jar and thus are not usually on the classpath.
 */
public class CompilerTreeApiTestRunner extends BlockJUnit4ClassRunner {
  private static final Supplier<TestClassLoader> TEST_CLASS_LOADER =
      MoreSuppliers.memoize(TestClassLoader::new);

  public CompilerTreeApiTestRunner(Class<?> klass) throws InitializationError {
    // The way runners work, the test class has already been loaded (that's how it knows which
    // runner to use). We'll ignore that one, though, and load the version from our class loader
    // which has access to the Compiler Tree API, in Java 8. This is hacky and wrong, but it's test
    // code. :-) In Java 9+, the Compiler Tree API is part of the JCL proper, so we can skip this
    // nonsense.
    super(reloadFromCompilerClassLoader(klass));
  }

  public static Class<?> reloadFromCompilerClassLoader(Class<?> clazz) throws InitializationError {
    if (JavaVersionUtils.getMajorVersion() >= 9) {
      return clazz;
    }
    try {
      return Class.forName(clazz.getName(), true, TEST_CLASS_LOADER.get());
    } catch (ClassNotFoundException e) {
      throw new InitializationError(e);
    }
  }

  private static class TestClassLoader extends URLClassLoader {
    public TestClassLoader() {
      super(getSystemClassLoaderUrls(), SynchronizedToolProvider.getSystemToolClassLoader());
    }

    private static URL[] getSystemClassLoaderUrls() {
      // Note: This only works on Java 8 and earlier, as in Java 9+, the system class loader is no
      // longer a URLClassLoader.
      Preconditions.checkState(JavaVersionUtils.getMajorVersion() < 9);
      URLClassLoader systemClassLoader = (URLClassLoader) ClassLoader.getSystemClassLoader();
      return systemClassLoader.getURLs();
    }

    @Override
    public Class<?> loadClass(String name) throws ClassNotFoundException {
      if (shouldLoadClass(name)) {
        // Under ordinary circumstances doing this is a gross violation of the ClassLoader contract,
        // because all of these classes can in fact be loaded by one of our parents. If someone
        // already did load them via one of our parents, we could get weird casting errors.
        //
        // However, this is test code, we're loading the test cases themselves with this
        // ClassLoader, so Everything Will Be Fine(tm).
        return findClass(name);
      }
      return super.loadClass(name);
    }
  }

  private static boolean shouldLoadClass(String name) {
    return name.startsWith("com.facebook.buck");
  }
}
