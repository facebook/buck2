/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.testutil.compiler;

import org.junit.runners.Parameterized;

/**
 * Parameterized test runner that enables tests to work with the Compiler Tree API implementation
 * corresponding to the compiler returned by {@link
 * javax.tools.ToolProvider#getSystemJavaCompiler()}. These are public APIs that are not provided in
 * rt.jar and thus are not usually on the classpath.
 */
public class CompilerTreeApiParameterized extends Parameterized {
  public CompilerTreeApiParameterized(Class<?> klass) throws Throwable {
    super(CompilerTreeApiTestRunner.reloadFromCompilerClassLoader(klass));
  }
}
