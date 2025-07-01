/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java;

import static com.facebook.buck.jvm.java.JavacLanguageLevelOptions.TARGETED_JAVA_VERSION;
import static org.junit.Assert.assertEquals;

import com.facebook.buck.core.filesystems.RelPath;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSortedSet;
import java.io.File;
import org.junit.Test;

public class Jsr199JavacTest {
  private static final RelPath PATH_TO_SRCS_LIST = RelPath.get("srcs_list");
  public static final ImmutableSortedSet<RelPath> SOURCE_FILES =
      ImmutableSortedSet.orderedBy(RelPath.comparator()).add(RelPath.get("foobar.java")).build();

  @Test
  public void testJavacCommand() {
    Jsr199Javac.ResolvedJsr199Javac firstOrder = createTestStep();
    Jsr199Javac.ResolvedJsr199Javac warn = createTestStep();
    Jsr199Javac.ResolvedJsr199Javac transitive = createTestStep();

    assertEquals(
        String.format(
            "javac -source %s -target %s -g -d . -classpath foo.jar @%s",
            TARGETED_JAVA_VERSION, TARGETED_JAVA_VERSION, PATH_TO_SRCS_LIST),
        firstOrder.getDescription(getArgs().add("foo.jar").build(), PATH_TO_SRCS_LIST));
    assertEquals(
        String.format(
            "javac -source %s -target %s -g -d . -classpath foo.jar @%s",
            TARGETED_JAVA_VERSION, TARGETED_JAVA_VERSION, PATH_TO_SRCS_LIST),
        warn.getDescription(getArgs().add("foo.jar").build(), PATH_TO_SRCS_LIST));
    assertEquals(
        String.format(
            "javac -source %s -target %s -g -d . -classpath bar.jar%sfoo.jar @%s",
            TARGETED_JAVA_VERSION, TARGETED_JAVA_VERSION, File.pathSeparator, PATH_TO_SRCS_LIST),
        transitive.getDescription(
            getArgs().add("bar.jar" + File.pathSeparator + "foo.jar").build(), PATH_TO_SRCS_LIST));
  }

  private Jsr199Javac.ResolvedJsr199Javac createTestStep() {
    return JdkProvidedInMemoryJavac.createJsr199Javac();
  }

  private ImmutableList.Builder<String> getArgs() {
    return ImmutableList.<String>builder()
        .add(
            "-source",
            TARGETED_JAVA_VERSION,
            "-target",
            TARGETED_JAVA_VERSION,
            "-g",
            "-d",
            ".",
            "-classpath");
  }
}
