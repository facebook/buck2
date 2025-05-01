/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.io.file;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.testutil.TemporaryPaths;
import com.google.common.collect.ImmutableList;
import com.google.common.jimfs.Configuration;
import com.google.common.jimfs.Jimfs;
import java.io.File;
import java.io.IOException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import org.hamcrest.Matchers;
import org.junit.Rule;
import org.junit.Test;

public class MostFilesTest {

  @Rule public TemporaryPaths tmp = new TemporaryPaths();

  @Test
  public void testDeleteRecursively() throws IOException {
    tmp.newFile(".dotfile");
    tmp.newFile("somefile");
    tmp.newFolder("foo");
    tmp.newFile("foo/bar");
    tmp.newFolder("foo/baz");
    tmp.newFile("foo/baz/biz");
    assertEquals("There should be files to delete.", 3, tmp.getRoot().toFile().listFiles().length);
    MostFiles.deleteRecursively(tmp.getRoot());
    assertNull(tmp.getRoot().toFile().listFiles());
  }

  @Test
  public void deleteRecursivelyIfExistsShouldNotFailOnNonExistentDir() throws IOException {
    FileSystem vfs = Jimfs.newFileSystem(Configuration.unix());
    Path fakeTmpDir = vfs.getPath("/tmp/fake-tmp-dir");
    MostFiles.deleteRecursivelyIfExists(fakeTmpDir.resolve("nonexistent"));
  }

  @Test
  public void deleteRecursivelyNonExistentDeleteContentsIgnoreNoSuchFile() throws IOException {
    FileSystem vfs = Jimfs.newFileSystem(Configuration.unix());
    Path fakeTmpDir = vfs.getPath("/tmp/fake-tmp-dir");
    MostFiles.deleteRecursivelyWithOptions(
        fakeTmpDir.resolve("nonexistent"),
        EnumSet.of(
            MostFiles.DeleteRecursivelyOptions.DELETE_CONTENTS_ONLY,
            MostFiles.DeleteRecursivelyOptions.IGNORE_NO_SUCH_FILE_EXCEPTION));
  }

  @Test(expected = IOException.class)
  public void deleteRecursivelyIfExistsShouldFailOnFileIfDeletingOnlyContents() throws IOException {
    FileSystem vfs = Jimfs.newFileSystem(Configuration.unix());
    Path fakeTmpDir = vfs.getPath("/tmp/fake-tmp-dir");
    Path fileToDelete = fakeTmpDir.resolve("delete-me");
    Files.createDirectories(fakeTmpDir);
    MostFiles.writeLinesToFile(ImmutableList.of(""), fileToDelete);

    MostFiles.deleteRecursivelyWithOptions(
        fileToDelete, EnumSet.of(MostFiles.DeleteRecursivelyOptions.DELETE_CONTENTS_ONLY));
  }

  @Test
  public void deleteRecursivelyIfExistsShouldNotFailOnFile() throws IOException {
    FileSystem vfs = Jimfs.newFileSystem(Configuration.unix());
    Path fakeTmpDir = vfs.getPath("/tmp/fake-tmp-dir");
    Path fileToDelete = fakeTmpDir.resolve("delete-me");
    Files.createDirectories(fakeTmpDir);
    MostFiles.writeLinesToFile(ImmutableList.of(""), fileToDelete);

    MostFiles.deleteRecursivelyWithOptions(
        fileToDelete, EnumSet.noneOf(MostFiles.DeleteRecursivelyOptions.class));
    assertThat(Files.exists(fileToDelete), is(false));
  }

  @Test
  public void deleteRecursivelyIfExistsDeletesDirectory() throws IOException {
    FileSystem vfs = Jimfs.newFileSystem(Configuration.unix());
    Path fakeTmpDir = vfs.getPath("/tmp/fake-tmp-dir");
    Path dirToDelete = fakeTmpDir.resolve("delete-me");
    Path childDir = dirToDelete.resolve("child-dir");
    Files.createDirectories(childDir);
    MostFiles.deleteRecursivelyIfExists(dirToDelete);
    assertThat(Files.exists(dirToDelete), is(false));
  }

  @Test
  public void deleteRecursivelyIfExistsDoesNotFollowSymlinks() throws IOException {
    FileSystem vfs = Jimfs.newFileSystem(Configuration.unix());
    Path linkTarget = vfs.getPath("/link-target");
    Path linkSource = vfs.getPath("/link-source");
    Files.createDirectory(linkTarget);
    Files.createSymbolicLink(linkSource, linkTarget);
    MostFiles.deleteRecursivelyIfExists(linkSource);
    // link is deleted, but target should still exist
    assertTrue(Files.exists(linkTarget));
    assertFalse(Files.exists(linkSource));
  }

  @Test
  public void deleteRecursivelyIfExistsDeletesBrokenSymlink() throws IOException {
    FileSystem vfs = Jimfs.newFileSystem(Configuration.unix());
    Path linkTarget = vfs.getPath("/link-target");
    Path linkSource = vfs.getPath("/link-source");
    Files.createSymbolicLink(linkSource, linkTarget);

    // sanity check
    assertTrue(Files.isSymbolicLink(linkSource));
    assertTrue(Files.exists(linkSource, LinkOption.NOFOLLOW_LINKS));
    assertFalse(Files.exists(linkSource));

    // this should not fail even if target does not exist
    MostFiles.deleteRecursivelyIfExists(linkSource);
    assertFalse(Files.isSymbolicLink(linkSource));
    assertFalse(Files.exists(linkSource, LinkOption.NOFOLLOW_LINKS));
    assertFalse(Files.exists(linkSource));
  }

  @Test
  public void deleteRecursivelyContentsOnlyLeavesParentDirectory() throws IOException {
    FileSystem vfs = Jimfs.newFileSystem(Configuration.unix());
    Path fakeTmpDir = vfs.getPath("/tmp/fake-tmp-dir");
    Path dirToDelete = fakeTmpDir.resolve("delete-me");
    Path childDir = dirToDelete.resolve("child-dir");
    Files.createDirectories(childDir);
    MostFiles.deleteRecursivelyWithOptions(
        dirToDelete, EnumSet.of(MostFiles.DeleteRecursivelyOptions.DELETE_CONTENTS_ONLY));
    assertThat(Files.exists(dirToDelete), is(true));
    assertThat(Files.exists(childDir), is(false));
  }

  @Test
  public void testWriteLinesToFile() throws IOException {
    AbsPath outputFile = tmp.newFile("output.txt");
    ImmutableList<String> lines =
        ImmutableList.of("The", "quick brown fox", "jumps over", "the lazy dog.");
    MostFiles.writeLinesToFile(lines, outputFile);

    List<String> observedLines = Files.readAllLines(outputFile.getPath(), UTF_8);
    assertEquals(lines, observedLines);
  }

  @Test
  public void testSortFilesByAccessTime() throws IOException {
    AbsPath dir = tmp.newFolder();
    AbsPath fileW = dir.resolve("w");
    AbsPath fileX = dir.resolve("x");
    AbsPath fileY = dir.resolve("y");
    AbsPath fileZ = dir.resolve("z");

    Files.write(fileW.getPath(), "w".getBytes(UTF_8));
    Files.write(fileX.getPath(), "x".getBytes(UTF_8));
    Files.write(fileY.getPath(), "y".getBytes(UTF_8));
    Files.write(fileZ.getPath(), "z".getBytes(UTF_8));

    Files.setAttribute(fileW.getPath(), "lastAccessTime", FileTime.fromMillis(9000));
    Files.setAttribute(fileX.getPath(), "lastAccessTime", FileTime.fromMillis(0));
    Files.setAttribute(fileY.getPath(), "lastAccessTime", FileTime.fromMillis(1000));
    Files.setAttribute(fileZ.getPath(), "lastAccessTime", FileTime.fromMillis(2000));

    File[] files = dir.toFile().listFiles();
    MostFiles.sortFilesByAccessTime(files);
    assertEquals(
        "Files short be sorted from most recently accessed to least recently accessed.",
        ImmutableList.of(fileW.toFile(), fileZ.toFile(), fileY.toFile(), fileX.toFile()),
        Arrays.asList(files));
  }

  @Test
  public void testMakeExecutable() throws IOException {
    AbsPath file = tmp.newFile();

    // If the file system does not support the executable permission, skip the test
    assumeTrue(file.toFile().setExecutable(false));
    assertFalse("File should not be executable", file.toFile().canExecute());
    MostFiles.makeExecutable(file);
    assertTrue("File should be executable", file.toFile().canExecute());

    assumeTrue(file.toFile().setExecutable(true));
    assertTrue("File should be executable", Files.isExecutable(file.getPath()));
    MostFiles.makeExecutable(file);
    assertTrue("File should be executable", Files.isExecutable(file.getPath()));
  }

  @Test
  public void testMakeExecutableOnPosix() throws IOException {
    assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"));

    AbsPath file = tmp.newFile();

    Files.setPosixFilePermissions(file.getPath(), PosixFilePermissions.fromString("r--------"));
    MostFiles.makeExecutable(file);
    assertEquals(
        "Owner's execute permission should have been set",
        "r-x------",
        PosixFilePermissions.toString(Files.getPosixFilePermissions(file.getPath())));

    Files.setPosixFilePermissions(file.getPath(), PosixFilePermissions.fromString("---r-----"));
    MostFiles.makeExecutable(file);
    assertEquals(
        "Group's execute permission should have been set",
        "---r-x---",
        PosixFilePermissions.toString(Files.getPosixFilePermissions(file.getPath())));

    Files.setPosixFilePermissions(file.getPath(), PosixFilePermissions.fromString("------r--"));
    MostFiles.makeExecutable(file);
    assertEquals(
        "Others' execute permission should have been set",
        "------r-x",
        PosixFilePermissions.toString(Files.getPosixFilePermissions(file.getPath())));

    Files.setPosixFilePermissions(file.getPath(), PosixFilePermissions.fromString("r--r--r--"));
    MostFiles.makeExecutable(file);
    assertEquals(
        "All execute permissions should have been set",
        "r-xr-xr-x",
        PosixFilePermissions.toString(Files.getPosixFilePermissions(file.getPath())));

    Files.setPosixFilePermissions(file.getPath(), PosixFilePermissions.fromString("r-xrw-rwx"));
    MostFiles.makeExecutable(file);
    assertEquals(
        "Only group's execute permission should have been set",
        "r-xrwxrwx",
        PosixFilePermissions.toString(Files.getPosixFilePermissions(file.getPath())));

    Files.setPosixFilePermissions(file.getPath(), PosixFilePermissions.fromString("-w---x-wx"));
    MostFiles.makeExecutable(file);
    assertEquals(
        "No permissions should have been changed",
        "-w---x-wx",
        PosixFilePermissions.toString(Files.getPosixFilePermissions(file.getPath())));

    Files.setPosixFilePermissions(file.getPath(), PosixFilePermissions.fromString("---------"));
    MostFiles.makeExecutable(file);
    assertEquals(
        "No permissions should have been changed",
        "---------",
        PosixFilePermissions.toString(Files.getPosixFilePermissions(file.getPath())));

    Files.setPosixFilePermissions(file.getPath(), PosixFilePermissions.fromString("rwxrwxrwx"));
    MostFiles.makeExecutable(file);
    assertEquals(
        "No permissions should have been changed",
        "rwxrwxrwx",
        PosixFilePermissions.toString(Files.getPosixFilePermissions(file.getPath())));
  }

  @Test
  public void concatenatingNoFilesReturnsFalse() throws IOException {
    FileSystem vfs = Jimfs.newFileSystem(Configuration.unix());
    Path outputPath = vfs.getPath("logs.txt");
    boolean collected = MostFiles.concatenateFiles(outputPath, ImmutableList.of());
    assertThat(collected, is(false));
    assertThat(Files.exists(outputPath), is(false));
  }

  @Test
  public void concatenatingTwoEmptyFilesReturnsFalse() throws Exception {
    FileSystem vfs = Jimfs.newFileSystem(Configuration.unix());

    Path fooPath = vfs.getPath("foo.txt");
    Files.write(fooPath, new byte[0]);

    Path barPath = vfs.getPath("bar.txt");
    Files.write(barPath, new byte[0]);

    Path outputPath = vfs.getPath("logs.txt");

    boolean concatenated =
        MostFiles.concatenateFiles(outputPath, ImmutableList.of(fooPath, barPath));
    assertThat(concatenated, is(false));

    assertThat(Files.exists(outputPath), is(false));
  }

  @Test
  public void concatenatingTwoNonEmptyFilesReturnsTrueAndWritesConcatenatedFile() throws Exception {
    FileSystem vfs = Jimfs.newFileSystem(Configuration.unix());

    Path fooPath = vfs.getPath("foo.txt");
    Files.write(fooPath, "hello world\n".getBytes(UTF_8));

    Path barPath = vfs.getPath("bar.txt");
    Files.write(barPath, "goodbye world\n".getBytes(UTF_8));

    Path outputPath = vfs.getPath("logs.txt");

    boolean concatenated =
        MostFiles.concatenateFiles(outputPath, ImmutableList.of(fooPath, barPath));
    assertThat(concatenated, is(true));

    assertThat(
        Files.readAllLines(outputPath, UTF_8),
        Matchers.equalTo(ImmutableList.of("hello world", "goodbye world")));
  }
}
