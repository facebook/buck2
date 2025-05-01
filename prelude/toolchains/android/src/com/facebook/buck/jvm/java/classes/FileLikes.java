/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.classes;

/** Utilities for common operations when working with {@link FileLike}s. */
public class FileLikes {

  /** {@link FileLike} name suffix that identifies it as a Java class file. */
  private static final String CLASS_NAME_SUFFIX = ".class";

  /** Utility class: do not instantiate. */
  private FileLikes() {}

  public static boolean isClassFile(FileLike fileLike) {
    return fileLike.getRelativePath().endsWith(CLASS_NAME_SUFFIX);
  }

  public static String getFileNameWithoutClassSuffix(FileLike fileLike) {
    String name = fileLike.getRelativePath();
    return name.substring(0, name.length() - CLASS_NAME_SUFFIX.length());
  }
}
