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

import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;

import com.facebook.buck.jvm.kotlin.ksp.FilteringClassLoader;
import com.facebook.buck.util.ClassLoaderCache;
import com.google.common.collect.ImmutableList;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * Tests that KSP2 processor classloaders are correctly cached via ClassLoaderCache when using
 * FilteringClassLoader as the parent. This verifies the caching behavior introduced to avoid
 * creating a fresh URLClassLoader per KSP2 invocation.
 */
public class Ksp2ClassLoaderCacheTest {
  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  private File processorJar1;
  private File processorJar2;
  private FilteringClassLoader filteringClassLoader;

  @Before
  public void setUp() throws IOException {
    processorJar1 = tempFolder.newFile("processor1.jar");
    processorJar2 = tempFolder.newFile("processor2.jar");

    // Create a FilteringClassLoader matching KSP2's configuration:
    // allowlisted prefixes route to the host classloader,
    // everything else routes to the platform classloader.
    filteringClassLoader =
        new FilteringClassLoader(
            this.getClass().getClassLoader(),
            ClassLoader.getPlatformClassLoader(),
            "com.google.devtools.ksp.",
            "kotlin.",
            "ksp.");
  }

  @Test
  public void sameProcessorClasspathReturnsCachedClassLoader() throws Exception {
    try (ClassLoaderCache cache = new ClassLoaderCache()) {
      ImmutableList<URL> processorClasspath =
          ImmutableList.of(processorJar1.toURI().toURL(), processorJar2.toURI().toURL());

      ClassLoader cl1 = cache.getClassLoaderForClassPath(filteringClassLoader, processorClasspath);
      ClassLoader cl2 = cache.getClassLoaderForClassPath(filteringClassLoader, processorClasspath);

      assertSame("Same classpath should return cached classloader", cl1, cl2);
    }
  }

  @Test
  public void differentProcessorClasspathReturnsNewClassLoader() throws Exception {
    try (ClassLoaderCache cache = new ClassLoaderCache()) {
      ImmutableList<URL> classpath1 = ImmutableList.of(processorJar1.toURI().toURL());
      ImmutableList<URL> classpath2 = ImmutableList.of(processorJar2.toURI().toURL());

      ClassLoader cl1 = cache.getClassLoaderForClassPath(filteringClassLoader, classpath1);
      ClassLoader cl2 = cache.getClassLoaderForClassPath(filteringClassLoader, classpath2);

      assertNotSame("Different classpath should return different classloader", cl1, cl2);
    }
  }

  @Test
  public void differentFilteringClassLoaderParentsCacheSeparately() throws Exception {
    try (ClassLoaderCache cache = new ClassLoaderCache()) {
      FilteringClassLoader otherFilteringClassLoader =
          new FilteringClassLoader(
              this.getClass().getClassLoader(),
              ClassLoader.getPlatformClassLoader(),
              "com.google.devtools.ksp.",
              "kotlin.",
              "ksp.");

      ImmutableList<URL> processorClasspath = ImmutableList.of(processorJar1.toURI().toURL());

      ClassLoader cl1 = cache.getClassLoaderForClassPath(filteringClassLoader, processorClasspath);
      ClassLoader cl2 =
          cache.getClassLoaderForClassPath(otherFilteringClassLoader, processorClasspath);

      // Different parent classloader instances = different cache entries
      assertNotSame(
          "Different parent classloaders should produce separate cache entries", cl1, cl2);
    }
  }
}
