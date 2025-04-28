/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.runner;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.jvm.java.version.utils.JavaVersionUtils;
import com.facebook.buck.testutil.TemporaryPaths;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.After;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class FileClassPathRunnerTest {
  @Rule public TemporaryPaths temporaryPaths = new TemporaryPaths();

  @Rule public ExpectedException thrown = ExpectedException.none();

  private String oldTestRunnerClassesPropertyValue;
  private String oldClassPathFilePropertyValue;

  @Before
  public void setUp() {
    oldTestRunnerClassesPropertyValue =
        System.getProperty(FileClassPathRunner.TESTRUNNER_CLASSES_PROPERTY);
    oldClassPathFilePropertyValue = System.getProperty(FileClassPathRunner.CLASSPATH_FILE_PROPERTY);
    System.clearProperty(FileClassPathRunner.TESTRUNNER_CLASSES_PROPERTY);
    System.clearProperty(FileClassPathRunner.CLASSPATH_FILE_PROPERTY);
  }

  @After
  public void tearDown() {
    if (oldTestRunnerClassesPropertyValue != null) {
      System.setProperty(
          FileClassPathRunner.TESTRUNNER_CLASSES_PROPERTY, oldTestRunnerClassesPropertyValue);
    }
    if (oldClassPathFilePropertyValue != null) {
      System.setProperty(
          FileClassPathRunner.CLASSPATH_FILE_PROPERTY, oldClassPathFilePropertyValue);
    }
  }

  @Test
  public void mainThrowsIfNoTestRunnerPropertyOnJava8()
      throws IOException, ReflectiveOperationException {
    Assume.assumeTrue(JavaVersionUtils.getMajorVersion() <= 8);
    thrown.expect(IllegalArgumentException.class);
    FileClassPathRunner.main(new String[] {"one"});
  }

  @Test
  public void getClassPathWithoutClassPathFile() throws IOException {
    AbsPath testRunnerPath = temporaryPaths.newFolder("testrunner");
    System.setProperty(FileClassPathRunner.TESTRUNNER_CLASSES_PROPERTY, testRunnerPath.toString());
    final String expectedClassPathProperty = testRunnerPath.toString();
    final URL[] expectedResult = new URL[] {testRunnerPath.toUri().toURL()};

    StringBuilder classPathProperty = new StringBuilder();
    URL[] result = FileClassPathRunner.getClassPath(classPathProperty);
    Assert.assertEquals(expectedClassPathProperty, classPathProperty.toString());
    Assert.assertArrayEquals(expectedResult, result);
  }

  @Test
  public void getClassPathWithExistingClassPathFile() throws IOException {
    Path classPathFile = whenClassPathFileExists();
    System.setProperty(
        FileClassPathRunner.TESTRUNNER_CLASSES_PROPERTY,
        temporaryPaths.newFolder("testrunner").toString());
    System.setProperty(FileClassPathRunner.CLASSPATH_FILE_PROPERTY, classPathFile.toString());

    Path[] expectedEntries = expectedClassPathEntries();
    final String expectedClassPathProperty =
        String.format(
            "%s:%s:%s",
            temporaryPaths.getRoot().resolve("testrunner"), expectedEntries[0], expectedEntries[1]);
    final URL[] expectedResult =
        new URL[] {
          temporaryPaths.getRoot().resolve("testrunner").toUri().toURL(),
          expectedEntries[0].toUri().toURL(),
          expectedEntries[1].toUri().toURL()
        };

    StringBuilder classPathProperty = new StringBuilder();
    URL[] result = FileClassPathRunner.getClassPath(classPathProperty);
    Assert.assertEquals(expectedClassPathProperty, classPathProperty.toString());
    Assert.assertArrayEquals(expectedResult, result);
  }

  @Test
  public void getClassPathWithNonExistingClassPathFile() throws IOException {
    thrown.expect(NoSuchFileException.class);
    System.setProperty(
        FileClassPathRunner.TESTRUNNER_CLASSES_PROPERTY,
        temporaryPaths.newFolder("testrunner").toString());
    System.setProperty(FileClassPathRunner.CLASSPATH_FILE_PROPERTY, "/path/doesnt/exist");

    StringBuilder classPathProperty = new StringBuilder();
    FileClassPathRunner.getClassPath(classPathProperty);
  }

  @Test
  public void getTestClassPathWhenFileDoesNotExist() throws IOException {
    thrown.expect(NoSuchFileException.class);
    FileClassPathRunner.getTestClassPath(Paths.get("/tmp/doesnotexist"));
  }

  @Test
  public void getTestClassPathWhenSomeEntriesDoNotExist() throws IOException {
    Path classPathFile =
        whenClassPathFileExistsWithFakeEntries(
            temporaryPaths.getRoot().resolve("lib3.jar").toString());

    final List<Path> expectedTestClassPath = Arrays.asList(expectedClassPathEntries());
    List<Path> testClassPath = FileClassPathRunner.getTestClassPath(classPathFile);
    Assert.assertEquals(expectedTestClassPath, testClassPath);
  }

  private Path whenClassPathFileExists() throws IOException {
    return whenClassPathFileExistsWithFakeEntries();
  }

  private Path[] expectedClassPathEntries() {
    return new Path[] {
      temporaryPaths.getRoot().resolve("lib1.jar").getPath(),
      temporaryPaths.getRoot().resolve("lib2.jar").getPath()
    };
  }

  private Path whenClassPathFileExistsWithFakeEntries(String... fakeClassPathEntries)
      throws IOException {
    List<String> classPathEntries = new ArrayList<>();
    classPathEntries.add(temporaryPaths.newFile("lib1.jar").toString());
    classPathEntries.add(temporaryPaths.newFile("lib2.jar").toString());
    AbsPath classpathFile = temporaryPaths.newFile("classpathFile");
    if (fakeClassPathEntries != null) {
      classPathEntries.addAll(Arrays.asList(fakeClassPathEntries));
    }
    Files.write(classpathFile.getPath(), classPathEntries);
    return classpathFile.getPath();
  }

  @Test
  public void constructArgsWhenOneArg() {
    final String[] expectedArgs = {};
    String[] args = FileClassPathRunner.constructArgs(new String[] {"one"});
    Assert.assertArrayEquals(expectedArgs, args);
  }

  @Test
  public void constructArgsWhenManyArgs() {
    final String[] expectedArgs = {"two"};
    String[] args = FileClassPathRunner.constructArgs(new String[] {"one", "two"});
    Assert.assertArrayEquals(expectedArgs, args);
  }

  @Test
  public void getPlatformClassLoaderOnJava8() {
    Assume.assumeTrue(JavaVersionUtils.getMajorVersion() <= 8);
    thrown.expect(AssertionError.class);
    FileClassPathRunner.findPlatformClassLoader();
  }

  @Test
  public void getPlatformClassLoaderOnJava9Plus() {
    Assume.assumeTrue(JavaVersionUtils.getMajorVersion() >= 9);
    ClassLoader loader = FileClassPathRunner.findPlatformClassLoader();
    Assert.assertNotNull(loader);
  }
}
