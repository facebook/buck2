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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import com.google.common.collect.ImmutableSet;
import java.nio.file.Paths;
import org.junit.Test;

public class ClasspathCheckerTest {
  @Test
  public void emptyClasspathIsNotValid() {
    ClasspathChecker classpathChecker =
        new ClasspathChecker(
            "/", ":", Paths::get, dir -> false, file -> false, (path, glob) -> ImmutableSet.of());
    assertThat(classpathChecker.validateClasspath(""), is(false));
  }

  @Test
  public void classpathWithNonExistingDirIsNotValid() {
    ClasspathChecker classpathChecker =
        new ClasspathChecker(
            "/", ":", Paths::get, dir -> false, file -> false, (path, glob) -> ImmutableSet.of());
    assertThat(classpathChecker.validateClasspath("does-not-exist"), is(false));
  }

  @Test
  public void classpathWithExistingDirIsValid() {
    ClasspathChecker classpathChecker =
        new ClasspathChecker(
            "/",
            ":",
            Paths::get,
            dir -> dir.equals(Paths.get("exists")),
            file -> false,
            (path, glob) -> ImmutableSet.of());
    assertThat(classpathChecker.validateClasspath("exists"), is(true));
  }

  @Test
  public void classpathWithBothNonAndExistingDirIsValid() {
    ClasspathChecker classpathChecker =
        new ClasspathChecker(
            "/",
            ":",
            Paths::get,
            dir -> dir.equals(Paths.get("exists")),
            file -> false,
            (path, glob) -> ImmutableSet.of());
    assertThat(classpathChecker.validateClasspath("does-not-exist:exists"), is(true));
  }

  @Test
  public void classpathWithNonExistingJarIsNotValid() {
    ClasspathChecker classpathChecker =
        new ClasspathChecker(
            "/", ":", Paths::get, dir -> false, file -> false, (path, glob) -> ImmutableSet.of());
    assertThat(classpathChecker.validateClasspath("does-not-exist.jar"), is(false));
  }

  @Test
  public void classpathWithExistingJarIsValid() {
    ClasspathChecker classpathChecker =
        new ClasspathChecker(
            "/",
            ":",
            Paths::get,
            dir -> false,
            file -> file.equals(Paths.get("exists.jar")),
            (path, glob) -> ImmutableSet.of());
    assertThat(classpathChecker.validateClasspath("exists.jar"), is(true));
  }

  @Test
  public void classpathWithExistingZipIsValid() {
    ClasspathChecker classpathChecker =
        new ClasspathChecker(
            "/",
            ":",
            Paths::get,
            dir -> false,
            file -> file.equals(Paths.get("exists.zip")),
            (path, glob) -> ImmutableSet.of());
    assertThat(classpathChecker.validateClasspath("exists.zip"), is(true));
  }

  @Test
  public void classpathWithExistingNonJarOrZipIsNotValid() {
    ClasspathChecker classpathChecker =
        new ClasspathChecker(
            "/",
            ":",
            Paths::get,
            dir -> false,
            file -> file.equals(Paths.get("exists.notajar")),
            (path, glob) -> ImmutableSet.of());
    assertThat(classpathChecker.validateClasspath("exists.notajar"), is(false));
  }

  @Test
  public void classpathWithEmptyGlobIsNotValid() {
    ClasspathChecker classpathChecker =
        new ClasspathChecker(
            "/", ":", Paths::get, dir -> false, file -> false, (path, glob) -> ImmutableSet.of());
    assertThat(classpathChecker.validateClasspath("*"), is(false));
  }

  @Test
  public void classpathWithJarGlobIsValid() {
    ClasspathChecker classpathChecker =
        new ClasspathChecker(
            "/",
            ":",
            Paths::get,
            dir -> false,
            file -> false,
            (path, glob) -> ImmutableSet.of(Paths.get("foo.jar")));
    assertThat(classpathChecker.validateClasspath("*"), is(true));
  }

  @Test
  public void classpathWithSubdirJarGlobIsValid() {
    ClasspathChecker classpathChecker =
        new ClasspathChecker(
            "/",
            ":",
            Paths::get,
            dir -> false,
            file -> false,
            (path, glob) -> ImmutableSet.of(Paths.get("foo/bar/foo.jar")));
    assertThat(classpathChecker.validateClasspath("foo/bar/*"), is(true));
  }
}
