/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.io.file;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.testutil.TemporaryPaths;
import com.facebook.buck.util.environment.Platform;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.junit.Rule;
import org.junit.Test;

public class MorePathsTest {

  @Rule public TemporaryPaths tmp = new TemporaryPaths();

  @Test
  public void testGetRelativePath() {
    // Path on base directory.
    assertEquals(
        RelPath.get("file"), MorePaths.getRelativePath(RelPath.get("file"), RelPath.get("")));

    // Path on base directory (using null).
    assertEquals(RelPath.get("file"), MorePaths.getRelativePath(RelPath.get("file"), null));

    // Path internal to base directory.
    assertEquals(
        RelPath.get("dir/file"),
        MorePaths.getRelativePath(RelPath.get("base/dir/file"), RelPath.get("base")));

    // Path external to base directory.
    assertEquals(
        RelPath.get("../dir1/file"),
        MorePaths.getRelativePath(RelPath.get("dir1/file"), RelPath.get("base")));
  }

  @Test
  public void testFilterForSubpaths() {
    AbsPath root = tmp.getRoot();
    Set<Path> paths =
        Stream.of(
                ".buckd",
                "foo/bar",
                root + "/buck-cache",
                Paths.get("/root/not/in/test").toAbsolutePath().toString())
            .map(Paths::get)
            .collect(Collectors.toSet());

    assertEquals(
        "Set should have been filtered down to paths contained under root.",
        ImmutableSet.of(Paths.get(".buckd"), Paths.get("foo/bar"), Paths.get("buck-cache")),
        MorePaths.filterForSubpaths(paths, root.getPath()));
  }

  @Test
  public void testCreateRelativeSymlinkToFilesInRoot() throws IOException {
    tmp.newFile("biz.txt");

    RelPath pathToDesiredLinkUnderProjectRoot = RelPath.get("gamma.txt");
    RelPath pathToExistingFileUnderProjectRoot = RelPath.get("biz.txt");
    RelPath relativePath =
        createRelativeSymlink(
            pathToDesiredLinkUnderProjectRoot, pathToExistingFileUnderProjectRoot, tmp.getRoot());

    AbsPath absolutePathToDesiredLinkUnderProjectRoot =
        tmp.getRoot().resolve(pathToDesiredLinkUnderProjectRoot);
    assertTrue(Files.isSymbolicLink(absolutePathToDesiredLinkUnderProjectRoot.getPath()));

    Path targetOfSymbolicLink =
        Files.readSymbolicLink(absolutePathToDesiredLinkUnderProjectRoot.getPath());

    validateSymlinklTarget(
        pathToExistingFileUnderProjectRoot,
        pathToDesiredLinkUnderProjectRoot,
        absolutePathToDesiredLinkUnderProjectRoot,
        targetOfSymbolicLink,
        relativePath);

    AbsPath absolutePathToExistingFileUnderProjectRoot =
        tmp.getRoot().resolve(pathToExistingFileUnderProjectRoot);
    Files.write(absolutePathToExistingFileUnderProjectRoot.getPath(), "Hello, World!".getBytes());
    String dataReadFromSymlink =
        new String(Files.readAllBytes(absolutePathToDesiredLinkUnderProjectRoot.getPath()));
    assertEquals("Hello, World!", dataReadFromSymlink);
  }

  @Test
  public void testCreateRelativeSymlinkToFileInRoot() throws IOException {
    tmp.newFile("biz.txt");

    tmp.newFolder("alpha", "beta");
    RelPath pathToDesiredLinkUnderProjectRoot = RelPath.get("alpha/beta/gamma.txt");
    RelPath pathToExistingFileUnderProjectRoot = RelPath.get("biz.txt");
    RelPath relativePath =
        createRelativeSymlink(
            pathToDesiredLinkUnderProjectRoot, pathToExistingFileUnderProjectRoot, tmp.getRoot());

    AbsPath absolutePathToDesiredLinkUnderProjectRoot =
        tmp.getRoot().resolve(pathToDesiredLinkUnderProjectRoot);
    assertTrue(Files.isSymbolicLink(absolutePathToDesiredLinkUnderProjectRoot.getPath()));

    Path targetOfSymbolicLink =
        Files.readSymbolicLink(absolutePathToDesiredLinkUnderProjectRoot.getPath());

    validateSymlinklTarget(
        pathToExistingFileUnderProjectRoot,
        pathToDesiredLinkUnderProjectRoot,
        absolutePathToDesiredLinkUnderProjectRoot,
        targetOfSymbolicLink,
        relativePath);

    AbsPath absolutePathToExistingFileUnderProjectRoot =
        tmp.getRoot().resolve(pathToExistingFileUnderProjectRoot);
    Files.write(absolutePathToExistingFileUnderProjectRoot.getPath(), "Hello, World!".getBytes());
    String dataReadFromSymlink =
        new String(Files.readAllBytes(absolutePathToDesiredLinkUnderProjectRoot.getPath()));
    assertEquals("Hello, World!", dataReadFromSymlink);
  }

  @Test
  public void testCreateRelativeSymlinkToFilesOfVaryingDepth() throws IOException {
    tmp.newFolder("foo", "bar", "baz");
    tmp.newFile("foo/bar/baz/biz.txt");

    tmp.newFolder("alpha", "beta");
    RelPath pathToDesiredLinkUnderProjectRoot = RelPath.get("alpha/beta/gamma.txt");
    RelPath pathToExistingFileUnderProjectRoot = RelPath.get("foo/bar/baz/biz.txt");
    RelPath relativePath =
        createRelativeSymlink(
            pathToDesiredLinkUnderProjectRoot, pathToExistingFileUnderProjectRoot, tmp.getRoot());

    AbsPath absolutePathToDesiredLinkUnderProjectRoot =
        tmp.getRoot().resolve(pathToDesiredLinkUnderProjectRoot);
    assertTrue(Files.isSymbolicLink(absolutePathToDesiredLinkUnderProjectRoot.getPath()));

    Path targetOfSymbolicLink =
        Files.readSymbolicLink(absolutePathToDesiredLinkUnderProjectRoot.getPath());

    validateSymlinklTarget(
        pathToExistingFileUnderProjectRoot,
        pathToDesiredLinkUnderProjectRoot,
        absolutePathToDesiredLinkUnderProjectRoot,
        targetOfSymbolicLink,
        relativePath);

    AbsPath absolutePathToExistingFileUnderProjectRoot =
        tmp.getRoot().resolve(pathToExistingFileUnderProjectRoot);
    Files.write(absolutePathToExistingFileUnderProjectRoot.getPath(), "Hello, World!".getBytes());
    String dataReadFromSymlink =
        new String(Files.readAllBytes(absolutePathToDesiredLinkUnderProjectRoot.getPath()));
    assertEquals("Hello, World!", dataReadFromSymlink);
  }

  @Test
  public void testExpandHomeDir() {
    Path homeDir = Paths.get(System.getProperty("user.home"));
    assertEquals(
        "Must expand home dir.",
        homeDir.resolve("foo"),
        MorePaths.expandHomeDir(Paths.get("~/foo")));

    assertEquals(
        "Must expand home dir by itself too.", homeDir, MorePaths.expandHomeDir(Paths.get("~")));

    Path relativePath = Paths.get("foo/bar");
    assertEquals(
        "Must not expand relative paths.", relativePath, MorePaths.expandHomeDir(relativePath));

    Path absolutePath = Paths.get("/foo/bar");
    assertEquals(
        "Must not expand absolute paths.", absolutePath, MorePaths.expandHomeDir(absolutePath));

    Path funnyHomePath = Paths.get("~jacko/foo");
    assertEquals(
        "Must only expand home paths starting with ~/",
        funnyHomePath,
        MorePaths.expandHomeDir(funnyHomePath));
  }

  @Test
  public void relativizeWithDotPathDoesNotAddExtraDotDotPath() {
    // Ensure workaround for bug JDK-6925169 for "." case.
    Path p1 = Paths.get("./a/b");
    Path p2 = Paths.get("c/d/e");
    assertThat(MorePaths.relativize(p1, p2), equalTo(Paths.get("../../c/d/e")));
  }

  @Test
  public void relativizeWithDotDotPathDoesNotAddExtraDotDotPath() {
    // Ensure workaround for bug JDK-6925169 for ".." case.
    Path p1 = Paths.get("a/../b");
    Path p2 = Paths.get("c/d/e");
    assertThat(MorePaths.relativize(p1, p2), equalTo(Paths.get("../c/d/e")));
  }

  @Test
  public void relativizeWithEmptyPath() {
    // Ensure workaround for Windows Path relativize bug
    Path p1 = Paths.get("");
    Path p2 = Paths.get("foo");
    assertThat(MorePaths.relativize(p1, p2), equalTo(Paths.get("foo")));
  }

  @Test
  public void normalizeWithEmptyPath() {
    // Ensure workaround for Java Path normalize bug
    Path emptyPath = Paths.get("");
    assertThat(MorePaths.normalize(emptyPath), equalTo(emptyPath));
  }

  @Test
  public void fixPathCommonTest() {
    Path inputPath = Paths.get("subdir/subdir2/foo/bar/x.file");
    Path expecting = Paths.get("subdir/subdir2/foo/bar/x.file");
    assertEquals(expecting, MorePaths.fixPath(inputPath));
  }

  @Test
  public void fixPathAbsoluteTest() {
    Path inputPath = Paths.get("/subdir/subdir2/foo/bar/x.file");
    Path expecting = Paths.get("/subdir/subdir2/foo/bar/x.file");
    assertEquals(expecting, MorePaths.fixPath(inputPath));
  }

  @Test
  public void fixPathEmptyPartTest() {
    Path inputPath = Paths.get("subdir/subdir2//foo///bar/////x.file");
    Path expecting = Paths.get("subdir/subdir2/foo/bar/x.file");
    assertEquals(expecting, MorePaths.fixPath(inputPath));
  }

  @Test
  public void fixPathDotPartTest() {
    Path inputPath = Paths.get("subdir/subdir2/./foo/././bar/./x.file");
    Path expecting = Paths.get("subdir/subdir2/foo/bar/x.file");
    assertEquals(expecting, MorePaths.fixPath(inputPath));
  }

  @Test
  public void fixPathDotDotPartTest() {
    Path inputPath = Paths.get("subdir/subdir2/foo/../bar/x.file");
    Path expecting = Paths.get("subdir/subdir2/foo/../bar/x.file");
    // should be the same!  Does not fully "normalize" path.
    assertEquals(expecting, MorePaths.fixPath(inputPath));
  }

  @Test
  public void getNameWithoutExtension() {
    Path inputPath = Paths.get("subdir/subdir2/bar/x.file");
    assertEquals("x", MorePaths.getNameWithoutExtension(inputPath));
  }

  private void validateSymlinklTarget(
      RelPath pathToExistingFileUnderProjectRoot,
      RelPath pathToDesiredLinkUnderProjectRoot,
      AbsPath absolutePathToDesiredLinkUnderProjectRoot,
      Path targetOfSymbolicLink,
      RelPath relativePath) {
    if (Platform.detect() == Platform.WINDOWS) {
      // On Windows Files.readSymbolicLink returns the absolute path to the target.
      RelPath relToLinkTargetPath =
          MorePaths.getRelativePath(
              pathToExistingFileUnderProjectRoot, pathToDesiredLinkUnderProjectRoot.getParent());
      AbsPath absolutePathToTargetUnderProjectRoot =
          absolutePathToDesiredLinkUnderProjectRoot
              .getParent()
              .resolve(relToLinkTargetPath)
              .normalize();
      assertEquals(absolutePathToTargetUnderProjectRoot, AbsPath.of(targetOfSymbolicLink));
    } else {
      assertEquals(relativePath, RelPath.of(targetOfSymbolicLink));
    }
  }

  @Test
  public void stripSuffix() {
    assertThat(
        MorePaths.stripSuffix(Paths.get("a/b/c"), Paths.get("c")),
        equalTo(Optional.of(Paths.get("a/b"))));
    assertThat(
        MorePaths.stripSuffix(Paths.get("a/b/c"), Paths.get("b/c")),
        equalTo(Optional.of(Paths.get("a"))));
    assertThat(
        MorePaths.stripSuffix(Paths.get("a/b/c"), Paths.get("a/b/c")),
        equalTo(Optional.of(Paths.get(""))));
    assertThat(
        MorePaths.stripSuffix(Paths.get("a/b/c"), Paths.get("d")), equalTo(Optional.empty()));
    assertThat(
        MorePaths.stripSuffix(Paths.get("a/b/c"), Paths.get("z/a/b/c")), equalTo(Optional.empty()));
  }

  private static RelPath createRelativeSymlink(
      RelPath pathToDesiredLinkUnderProjectRoot,
      RelPath pathToExistingFileUnderProjectRoot,
      AbsPath root)
      throws IOException {

    RelPath target =
        MorePaths.getRelativePath(
            pathToExistingFileUnderProjectRoot, pathToDesiredLinkUnderProjectRoot.getParent());

    Files.createSymbolicLink(
        root.resolve(pathToDesiredLinkUnderProjectRoot).getPath(), target.getPath());

    return target;
  }
}
