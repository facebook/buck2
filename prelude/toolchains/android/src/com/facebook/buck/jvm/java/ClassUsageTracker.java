/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.google.common.collect.ImmutableMap;
import java.net.URI;
import java.nio.file.Path;
import java.util.Map;
import javax.tools.FileObject;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;

/**
 * Tracks which classes are actually read by the compiler by providing a special {@link
 * JavaFileManager}.
 */
class ClassUsageTracker implements FileManagerListener {

  private final ClassUsageURIParser parser = new ClassUsageURIParser();

  /**
   * Returns a map from JAR path on disk to .class file paths within the jar for any classes that
   * were used, and the count for how often those classes were read.
   */
  public ImmutableMap<Path, Map<Path, Integer>> getClassUsageMap() {
    return parser.getClassUsageMap();
  }

  @Override
  public void onFileRead(FileObject fileObject) {
    if (!(fileObject instanceof JavaFileObject)) {
      return;
    }
    JavaFileObject javaFileObject = (JavaFileObject) fileObject;
    URI classFileJarUri = javaFileObject.toUri();
    parser.parseAndRecordURI(classFileJarUri);
  }

  @Override
  public void onFileWritten(FileObject file) {}
}
