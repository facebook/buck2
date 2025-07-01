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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.java.javax.SynchronizedToolProvider;
import com.facebook.buck.testutil.TemporaryPaths;
import com.facebook.buck.util.ClassLoaderCache;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;
import java.io.IOException;
import org.junit.Rule;
import org.junit.Test;

public class AnnotationProcessorFactoryTest {

  @Rule public TemporaryPaths tmp = new TemporaryPaths();

  @Test
  public void testAnnotationProcessorClassloadersNotReusedIfMarkedUnsafe() {
    assertFalse(
        isAnnotationProcessorClassLoaderReused(
            "some.Processor", // processor
            false)); // safe processors
  }

  @Test
  public void testAnnotationProcessorClassloadersReusedIfMarkedSafe() {
    assertTrue(
        isAnnotationProcessorClassLoaderReused(
            "some.Processor", // processor
            true)); // safe processors
  }

  private boolean isAnnotationProcessorClassLoaderReused(
      String annotationProcessor, boolean canReuseClasspath) {
    RelPath classpath = RelPath.get("some/path/to.jar");
    ClassLoader baseClassLoader = SynchronizedToolProvider.getSystemToolClassLoader();
    ClassLoaderCache classLoaderCache = new ClassLoaderCache();
    String buildTarget = "//:test";
    AbsPath rootPath = tmp.getRoot();
    ResolvedJavacPluginProperties processorGroup =
        new ResolvedJavacPluginProperties(
            canReuseClasspath,
            false /* doesNotAffectAbi */,
            false /* supportsAbiGenerationFromSource */,
            false /* runsOnJavaOnly */,
            ImmutableSortedSet.of(annotationProcessor),
            ImmutableList.of(classpath),
            ImmutableMap.of() /* pathParams */,
            ImmutableList.of() /* arguments */);

    try (AnnotationProcessorFactory factory1 =
            new AnnotationProcessorFactory(baseClassLoader, classLoaderCache, buildTarget);
        AnnotationProcessorFactory factory2 =
            new AnnotationProcessorFactory(baseClassLoader, classLoaderCache, buildTarget)) {
      ClassLoader classLoader1 = factory1.getClassLoaderForProcessorGroup(processorGroup, rootPath);
      ClassLoader classLoader2 = factory2.getClassLoaderForProcessorGroup(processorGroup, rootPath);
      return classLoader1 == classLoader2;
    } catch (IOException e) {
      throw new AssertionError(e);
    }
  }
}
