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

import static org.junit.Assert.assertTrue;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.jvm.java.ClassUsageFileWriter;
import com.facebook.buck.jvm.java.DefaultClassUsageFileWriter;
import com.facebook.buck.jvm.java.MergingClassUsageFileWriter;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.ClasspathChanges;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlinSourceChanges;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode;
import com.google.common.collect.ImmutableList;
import org.junit.Test;

public class ClassUsageFileWriterFactoryTest {

  @Test
  public void when_incremental_mode_then_merging() {
    KotlincMode mode =
        new KotlincMode.Incremental(
            AbsPath.get("/"),
            AbsPath.get("/"),
            AbsPath.get("/"),
            KotlinSourceChanges.ToBeCalculated.INSTANCE,
            new ClasspathChanges.NoChanges(ImmutableList.of()),
            AbsPath.get("/"),
            null);

    ClassUsageFileWriter classUsageFileWriter = ClassUsageFileWriterFactory.create(mode);

    assertTrue(classUsageFileWriter instanceof MergingClassUsageFileWriter);
  }

  @Test
  public void when_non_incremental_mode_then_default() {
    KotlincMode mode = KotlincMode.NonIncremental.INSTANCE;

    ClassUsageFileWriter classUsageFileWriter = ClassUsageFileWriterFactory.create(mode);

    assertTrue(classUsageFileWriter instanceof DefaultClassUsageFileWriter);
  }
}
