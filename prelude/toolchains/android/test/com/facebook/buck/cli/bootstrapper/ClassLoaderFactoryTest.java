/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.cli.bootstrapper;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.arrayWithSize;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasItemInArray;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.object.HasToString.hasToString;
import static org.junit.Assert.assertThrows;

import java.net.URL;
import java.net.URLClassLoader;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;

public class ClassLoaderFactoryTest {

  static final String CLASS_PATH_JAR = "/classPath.jar";
  static final String EXTRA_CLASS_PATH_JAR = "/extraClassPath.jar";

  private final Map<String, String> testEnvironment = new HashMap<>();
  private final ClassLoaderFactory classLoaderFactory =
      new ClassLoaderFactory(testEnvironment::get);

  @Before
  public void setUp() {
    testEnvironment.clear();
  }

  @Test
  public void testMissingBuckClassPlath() {
    String expectedMessage = ClassLoaderFactory.BUCK_CLASSPATH + " not set";
    assertThrows(expectedMessage, RuntimeException.class, classLoaderFactory::create);
  }

  @Test
  public void testBuckClassPlath() {
    testEnvironment.put(ClassLoaderFactory.BUCK_CLASSPATH, CLASS_PATH_JAR);

    URL[] urls = ((URLClassLoader) classLoaderFactory.create()).getURLs();

    assertThat(urls, arrayWithSize(1));
    assertThat(urls, hasItemInArray(hasToString(containsString(CLASS_PATH_JAR))));
    assertThat(urls, not(hasItemInArray(hasToString(containsString(EXTRA_CLASS_PATH_JAR)))));
  }

  @Test
  public void testBuckClassPlathWithExtraClassPath() {
    testEnvironment.put(ClassLoaderFactory.BUCK_CLASSPATH, CLASS_PATH_JAR);
    testEnvironment.put(ClassLoaderFactory.EXTRA_BUCK_CLASSPATH, EXTRA_CLASS_PATH_JAR);

    URL[] urls = ((URLClassLoader) classLoaderFactory.create()).getURLs();

    assertThat(urls, arrayWithSize(2));
    assertThat(urls, hasItemInArray(hasToString(containsString(CLASS_PATH_JAR))));
    assertThat(urls, hasItemInArray(hasToString(containsString(EXTRA_CLASS_PATH_JAR))));
  }
}
