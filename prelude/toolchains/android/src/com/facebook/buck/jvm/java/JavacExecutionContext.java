/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.core.util.immutables.BuckStyleValue;
import com.facebook.buck.util.ClassLoaderCache;
import com.facebook.buck.util.ProcessExecutor;
import com.facebook.buck.util.Verbosity;
import com.google.common.collect.ImmutableMap;
import java.io.PrintStream;

@BuckStyleValue
public interface JavacExecutionContext {

  PrintStream getStdErr();

  ClassLoaderCache getClassLoaderCache();

  Verbosity getVerbosity();

  AbsPath getRuleCellRoot();

  ImmutableMap<String, String> getEnvironment();

  ProcessExecutor getProcessExecutor();

  RelPath getConfiguredBuckOut();
}
