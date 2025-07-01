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

import com.facebook.buck.util.zip.JarBuilder;
import java.io.IOException;
import java.nio.file.Path;
import org.objectweb.asm.ClassVisitor;

/** Provides access to bytecode produced by a {@link TestCompiler}. */
public interface Classes {
  void acceptClassVisitor(String qualifiedName, int flags, ClassVisitor cv) throws IOException;

  void createJar(Path jarPath, boolean hashEntries) throws IOException;

  void writeToJar(JarBuilder builder) throws IOException;
}
