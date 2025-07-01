/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.jvm.java.ClassUsageFileWriter;
import com.facebook.buck.jvm.java.DefaultClassUsageFileWriter;
import com.facebook.buck.jvm.java.MergingClassUsageFileWriter;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode;

public class ClassUsageFileWriterFactory {

  private ClassUsageFileWriterFactory() {}

  public static ClassUsageFileWriter create(KotlincMode mode) {
    if (mode instanceof KotlincMode.Incremental) {
      AbsPath classUsageFile = ((KotlincMode.Incremental) mode).getKotlinClassUsageFile();
      return new MergingClassUsageFileWriter(classUsageFile);
    } else {
      return new DefaultClassUsageFileWriter();
    }
  }
}
