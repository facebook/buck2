/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.unarchive;

import static com.google.common.collect.Iterables.concat;
import static org.hamcrest.MatcherAssert.assertThat;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.testutil.TemporaryPaths;
import com.facebook.buck.testutil.integration.TestDataHelper;
import com.facebook.buck.util.PatternsMatcher;
import com.facebook.buck.util.environment.Platform;
import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.nio.file.attribute.PosixFilePermission;
import java.time.Instant;
import java.util.List;
import java.util.Optional;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.hamcrest.Matchers;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

public class UntarTest {

  @Rule public TemporaryPaths tmpFolder = new TemporaryPaths();

  // Windows writes files by default as executable...
  private static final boolean FilesAreExecutableByDefault = Platform.detect() == Platform.WINDOWS;

  private static final Path OUTPUT_SUBDIR = Paths.get("output_dir");
  private static final FileTime expectedModifiedTime =
      FileTime.from(Instant.parse("2018-02-02T20:29:13Z"));
  private static final String mainDotJava =
      "class Main { public static void main(String[] args) { return; } }";
  private static final String otherDotJava =
      "class Other { public static void main(String[] args) { return; } }";
  private static final String echoDotSh = "#!/bin/sh\necho 'testing'";

  private AbsPath rootPath;

  @Before
  public void setUp() {
    rootPath = tmpFolder.getRoot();
  }

  private Path getTestFilePath(String extension) {
    Path testDataDirectory = TestDataHelper.getTestDataDirectory(this.getClass());
    return testDataDirectory.resolve("output" + extension);
  }

  private Path getDestPath(String component, String... components) {
    return OUTPUT_SUBDIR.resolve(Paths.get(component, components));
  }

  /** Assert that the modification time equals {@code expectedModifiedTime} for the given paths */
  private void assertModifiedTime(Iterable<Path> paths) throws IOException {
    for (Path path : paths) {
      assertModifiedTime(path);
    }
  }

  private void assertModifiedTime(Path path) throws IOException {
    AbsPath fullPath = rootPath.resolve(path);
    FileTime lastModifiedTime = Files.getLastModifiedTime(fullPath.getPath());
    Assert.assertEquals(
        String.format(
            "Expected %s to be modified at %s, but it was modified at %s",
            fullPath, expectedModifiedTime, lastModifiedTime),
        expectedModifiedTime,
        lastModifiedTime);
  }

  /** Assert that files exist and should/shouldn't be executable */
  private void assertExecutable(Iterable<Path> paths, boolean shouldBeExecutable) {
    for (Path path : paths) {
      assertExecutable(path, shouldBeExecutable);
    }
  }

  /** Assert that a file exists and should/shouldn't be executable */
  private void assertExecutable(Path path, boolean shouldBeExecutable) {
    assertExecutable(path, shouldBeExecutable, FilesAreExecutableByDefault);
  }

  private void assertExecutable(
      Path path, boolean shouldBeExecutable, boolean allowFilesToBeWrong) {
    AbsPath fullPath = rootPath.resolve(path);
    File file = fullPath.toFile();
    boolean isExecutable = file.canExecute();

    // Bit of a hack around windows writing normal files as exectuable by default
    if (!file.isDirectory() && isExecutable && allowFilesToBeWrong && !shouldBeExecutable) {
      return;
    }

    Assert.assertEquals(
        String.format(
            "Expected execute on %s to be %s, got %s", fullPath, shouldBeExecutable, isExecutable),
        shouldBeExecutable,
        isExecutable);
  }

  /** Assert that a directory exists inside of the temp directory */
  private void assertOutputDirExists(Path path) {
    AbsPath fullPath = rootPath.resolve(path);
    Assert.assertTrue(
        String.format("Expected %s to be a directory", fullPath),
        Files.isDirectory(fullPath.getPath()));
  }

  /** Assert that a file exists inside of the temp directory with given contents */
  private void assertOutputFileExists(Path path, String expectedContents) throws IOException {
    AbsPath fullPath = rootPath.resolve(path);
    Assert.assertTrue(
        String.format("Expected %s to be a file", fullPath),
        Files.isRegularFile(fullPath.getPath()));

    String contents = Joiner.on('\n').join(Files.readAllLines(fullPath.getPath()));
    Assert.assertEquals(expectedContents, contents);
  }

  /**
   * Assert that a symlink exists inside of the temp directory with given contents and that links to
   * the right file
   */
  private void assertOutputSymlinkExists(
      Path symlinkPath, Path expectedLinkedToPath, String expectedContents) throws IOException {
    AbsPath fullPath = rootPath.resolve(symlinkPath);
    if (Platform.detect() != Platform.WINDOWS) {
      Assert.assertTrue(
          String.format("Expected %s to be a symlink", fullPath),
          Files.isSymbolicLink(fullPath.getPath()));
      Path linkedToPath = Files.readSymbolicLink(fullPath.getPath());
      Assert.assertEquals(
          String.format(
              "Expected symlink at %s to point to %s, not %s",
              symlinkPath, expectedLinkedToPath, linkedToPath),
          expectedLinkedToPath,
          linkedToPath);
    }

    AbsPath realExpectedLinkedToPath =
        rootPath.resolve(symlinkPath.getParent().resolve(expectedLinkedToPath).normalize());
    Assert.assertTrue(
        String.format(
            "Expected link %s to be the same file as %s", fullPath, realExpectedLinkedToPath),
        Files.isSameFile(fullPath.getPath(), realExpectedLinkedToPath.getPath()));

    String contents = Joiner.on('\n').join(Files.readAllLines(fullPath.getPath()));
    Assert.assertEquals(expectedContents, contents);
  }

  @Test
  public void extractsTarFiles() throws IOException {
    extractsFiles(ArchiveFormat.TAR, Optional.empty());
  }

  @Test
  public void extractsTarGzFiles() throws IOException {
    extractsFiles(ArchiveFormat.TAR_GZ, Optional.empty());
  }

  @Test
  public void extractsTarXzFiles() throws IOException {
    extractsFiles(ArchiveFormat.TAR_XZ, Optional.empty());
  }

  @Test
  public void extractsTarBz2Files() throws IOException {
    extractsFiles(ArchiveFormat.TAR_BZ2, Optional.empty());
  }

  @Test
  public void extractsTarFilesWindowsStyle() throws IOException {
    extractsFiles(ArchiveFormat.TAR, Optional.of(true));
  }

  @Test
  public void extractsTarGzFilesWindowsStyle() throws IOException {
    extractsFiles(ArchiveFormat.TAR_GZ, Optional.of(true));
  }

  @Test
  public void extractsTarXzFilesWindowsStyle() throws IOException {
    extractsFiles(ArchiveFormat.TAR_XZ, Optional.of(true));
  }

  @Test
  public void extractsTarBz2FilesWindowsStyle() throws IOException {
    extractsFiles(ArchiveFormat.TAR_BZ2, Optional.of(true));
  }

  @Test
  public void extractsTarFilesPosixStyle() throws IOException {
    Assume.assumeThat(Platform.detect(), Matchers.not(Platform.WINDOWS));
    extractsFiles(ArchiveFormat.TAR, Optional.of(false));
  }

  @Test
  public void extractsTarGzFilesPosixStyle() throws IOException {
    Assume.assumeThat(Platform.detect(), Matchers.not(Platform.WINDOWS));
    extractsFiles(ArchiveFormat.TAR_GZ, Optional.of(false));
  }

  @Test
  public void extractsTarXzFilesPosixStyle() throws IOException {
    Assume.assumeThat(Platform.detect(), Matchers.not(Platform.WINDOWS));
    extractsFiles(ArchiveFormat.TAR_XZ, Optional.of(false));
  }

  @Test
  public void extractsTarBz2FilesPosixStyle() throws IOException {
    Assume.assumeThat(Platform.detect(), Matchers.not(Platform.WINDOWS));
    extractsFiles(ArchiveFormat.TAR_BZ2, Optional.of(false));
  }

  private void extractsFiles(ArchiveFormat format, Optional<Boolean> writeSymlinksLast)
      throws IOException {
    ImmutableList<Path> expectedPaths =
        ImmutableList.of(
            getDestPath("root", "echo.sh"),
            getDestPath("root", "alternative", "Main.java"),
            getDestPath("root", "alternative", "Link.java"),
            getDestPath("root", "src", "com", "facebook", "buck", "Main.java"),
            getDestPath("root_sibling", "Other.java"));

    ImmutableList.Builder<Path> expectedDirsBuilder = ImmutableList.builder();
    expectedDirsBuilder.add(OUTPUT_SUBDIR);
    expectedDirsBuilder.add(getDestPath("root"));
    expectedDirsBuilder.add(getDestPath("root", "alternative"));
    expectedDirsBuilder.add(getDestPath("root", "src"));
    expectedDirsBuilder.add(getDestPath("root", "src", "com"));
    expectedDirsBuilder.add(getDestPath("root", "src", "com", "facebook"));
    expectedDirsBuilder.add(getDestPath("root", "src", "com", "facebook", "buck"));
    expectedDirsBuilder.add(getDestPath("root_sibling"));
    expectedDirsBuilder.add(getDestPath("root", "empty_dir"));
    ImmutableList<Path> expectedDirs = expectedDirsBuilder.build();

    Path archivePath = getTestFilePath(format.getExtension());
    Untar unarchiver = (Untar) format.getUnarchiver();
    ImmutableSet<Path> unarchivedFiles;
    if (writeSymlinksLast.isPresent()) {
      unarchivedFiles =
          unarchiver.extractArchive(
              archivePath,
              rootPath,
              Paths.get("output_dir"),
              Optional.empty(),
              ExistingFileMode.OVERWRITE_AND_CLEAN_DIRECTORIES,
              PatternsMatcher.NONE,
              writeSymlinksLast.get());
    } else {
      unarchivedFiles =
          ImmutableSet.copyOf(
              unarchiver.extractArchive(
                  rootPath,
                  archivePath,
                  Paths.get("output_dir"),
                  ExistingFileMode.OVERWRITE_AND_CLEAN_DIRECTORIES));
    }

    assertThat(unarchivedFiles, Matchers.containsInAnyOrder(expectedPaths.toArray()));
    Assert.assertEquals(expectedPaths.size(), unarchivedFiles.size());

    // Make sure we wrote the files
    assertOutputFileExists(expectedPaths.get(0), echoDotSh);
    assertOutputSymlinkExists(expectedPaths.get(1), Paths.get("Link.java"), mainDotJava);
    assertOutputSymlinkExists(
        expectedPaths.get(2),
        Paths.get("..", "src", "com", "facebook", "buck", "Main.java"),
        mainDotJava);
    assertOutputFileExists(expectedPaths.get(3), mainDotJava);
    assertOutputFileExists(expectedPaths.get(4), otherDotJava);

    // Make sure we make the dirs
    for (Path dir : expectedDirs) {
      assertOutputDirExists(dir);
      // Dest dir is created by buck, doesn't come from the archive
      if (!dir.equals(OUTPUT_SUBDIR)) {
        assertModifiedTime(dir);
      }
    }

    // Make sure that we set modified time and execute bit properly
    assertModifiedTime(expectedPaths);
    assertExecutable(expectedPaths.get(0), true);
    if (rootPath.getFileSystem().supportedFileAttributeViews().contains("posix")) {
      AbsPath executablePath = rootPath.resolve(expectedPaths.get(0));
      assertThat(
          Files.getPosixFilePermissions(executablePath.getPath()),
          Matchers.hasItems(
              PosixFilePermission.OWNER_EXECUTE,
              PosixFilePermission.OTHERS_EXECUTE,
              PosixFilePermission.GROUP_EXECUTE));
    }
    assertExecutable(expectedPaths.subList(1, expectedPaths.size()), false);
  }

  @Test
  public void extractsFilesWithStrippedPrefix() throws IOException {
    ArchiveFormat format = ArchiveFormat.TAR;

    ImmutableList<Path> expectedPaths =
        ImmutableList.of(
            getDestPath("echo.sh"),
            getDestPath("alternative", "Main.java"),
            getDestPath("alternative", "Link.java"),
            getDestPath("src", "com", "facebook", "buck", "Main.java"));

    ImmutableList.Builder<Path> expectedDirsBuilder = ImmutableList.builder();
    expectedDirsBuilder.add(OUTPUT_SUBDIR);
    expectedDirsBuilder.add(getDestPath("alternative"));
    expectedDirsBuilder.add(getDestPath("src"));
    expectedDirsBuilder.add(getDestPath("src", "com"));
    expectedDirsBuilder.add(getDestPath("src", "com", "facebook"));
    expectedDirsBuilder.add(getDestPath("src", "com", "facebook", "buck"));
    expectedDirsBuilder.add(getDestPath("empty_dir"));
    ImmutableList<Path> expectedDirs = expectedDirsBuilder.build();

    Path archivePath = getTestFilePath(format.getExtension());
    ImmutableSet<Path> unarchivedFiles =
        format
            .getUnarchiver()
            .extractArchive(
                archivePath,
                rootPath,
                Paths.get("output_dir"),
                Optional.of(Paths.get("root")),
                PatternsMatcher.NONE,
                ExistingFileMode.OVERWRITE_AND_CLEAN_DIRECTORIES);

    assertThat(unarchivedFiles, Matchers.containsInAnyOrder(expectedPaths.toArray()));
    Assert.assertEquals(expectedPaths.size(), unarchivedFiles.size());

    // Make sure we wrote the files
    assertOutputFileExists(expectedPaths.get(0), echoDotSh);
    assertOutputSymlinkExists(expectedPaths.get(1), Paths.get("Link.java"), mainDotJava);
    assertOutputSymlinkExists(
        expectedPaths.get(2),
        Paths.get("..", "src", "com", "facebook", "buck", "Main.java"),
        mainDotJava);
    assertOutputFileExists(expectedPaths.get(3), mainDotJava);

    // Make sure we make the dirs
    for (Path dir : expectedDirs) {
      assertOutputDirExists(dir);
      // Dest dir is created by buck, doesn't come from the archive
      if (!dir.equals(OUTPUT_SUBDIR)) {
        assertModifiedTime(dir);
      }
    }

    // Make sure that we set modified time and execute bit properly
    assertModifiedTime(expectedPaths);
    assertExecutable(expectedPaths.get(0), true);
    assertExecutable(expectedPaths.subList(1, expectedPaths.size()), false);

    Path siblingDirPath = getDestPath("root_sibling");
    Path siblingFilePath = getDestPath("root_sibling", "Other.java");
    Path siblingFile2Path = getDestPath("Other.java");
    Assert.assertFalse(
        String.format("Expected %s to not exist", siblingDirPath), Files.exists(siblingDirPath));
    Assert.assertFalse(
        String.format("Expected %s to not exist", siblingFilePath), Files.exists(siblingFilePath));
    Assert.assertFalse(
        String.format("Expected %s to not exist", siblingFile2Path),
        Files.exists(siblingFile2Path));
  }

  @Test
  public void deletesExistingFilesInPathIfShouldBeDirectories() throws IOException {
    ArchiveFormat format = ArchiveFormat.TAR;

    ImmutableList<Path> expectedPaths =
        ImmutableList.of(
            getDestPath("root", "echo.sh"),
            getDestPath("root", "alternative", "Main.java"),
            getDestPath("root", "alternative", "Link.java"),
            getDestPath("root", "src", "com", "facebook", "buck", "Main.java"),
            getDestPath("root_sibling", "Other.java"));

    ImmutableList<Path> expectedDirs = ImmutableList.of(OUTPUT_SUBDIR, getDestPath("root"));

    Files.createDirectories(rootPath.resolve(OUTPUT_SUBDIR).getPath());
    Files.writeString(rootPath.resolve(getDestPath("root")).getPath(), "testing");
    assertOutputFileExists(getDestPath("root"), "testing");

    Path archivePath = getTestFilePath(format.getExtension());
    ImmutableList<Path> unarchivedFiles =
        format
            .getUnarchiver()
            .extractArchive(
                rootPath,
                archivePath,
                Paths.get("output_dir"),
                ExistingFileMode.OVERWRITE_AND_CLEAN_DIRECTORIES);

    assertThat(unarchivedFiles, Matchers.containsInAnyOrder(expectedPaths.toArray()));
    Assert.assertEquals(expectedPaths.size(), unarchivedFiles.size());

    assertOutputDirExists(expectedDirs.get(1));
    assertModifiedTime(expectedDirs.get(1));

    // Make sure we wrote the files
    assertOutputFileExists(expectedPaths.get(0), echoDotSh);
    assertOutputSymlinkExists(expectedPaths.get(1), Paths.get("Link.java"), mainDotJava);
    assertOutputSymlinkExists(
        expectedPaths.get(2),
        Paths.get("..", "src", "com", "facebook", "buck", "Main.java"),
        mainDotJava);
    assertOutputFileExists(expectedPaths.get(3), mainDotJava);
    assertOutputFileExists(expectedPaths.get(4), otherDotJava);

    // Make sure that we set modified time and execute bit properly
    assertModifiedTime(expectedPaths);
    assertExecutable(expectedPaths.get(0), true);
    assertExecutable(expectedPaths.subList(1, expectedPaths.size()), false);
  }

  @Test
  public void overwritesExistingFilesIfPresent() throws IOException {
    ArchiveFormat format = ArchiveFormat.TAR;

    ImmutableList<Path> expectedPaths =
        ImmutableList.of(
            getDestPath("root", "echo.sh"),
            getDestPath("root", "alternative", "Main.java"),
            getDestPath("root", "alternative", "Link.java"),
            getDestPath("root", "src", "com", "facebook", "buck", "Main.java"),
            getDestPath("root_sibling", "Other.java"));

    ImmutableList<Path> expectedDirs = ImmutableList.of(OUTPUT_SUBDIR, getDestPath("root"));

    Files.createDirectories(rootPath.resolve(OUTPUT_SUBDIR).getPath());
    Files.createDirectories(rootPath.resolve(getDestPath("root")).getPath());
    Files.writeString(rootPath.resolve(getDestPath("root", "echo.sh")).getPath(), "testing");
    assertOutputFileExists(getDestPath("root", "echo.sh"), "testing");

    Path archivePath = getTestFilePath(format.getExtension());
    ImmutableList<Path> unarchivedFiles =
        format
            .getUnarchiver()
            .extractArchive(
                rootPath,
                archivePath,
                Paths.get("output_dir"),
                ExistingFileMode.OVERWRITE_AND_CLEAN_DIRECTORIES);

    assertThat(unarchivedFiles, Matchers.containsInAnyOrder(expectedPaths.toArray()));
    Assert.assertEquals(expectedPaths.size(), unarchivedFiles.size());

    assertOutputDirExists(expectedDirs.get(1));
    assertModifiedTime(expectedDirs.get(1));

    // Make sure we wrote the files
    assertOutputFileExists(expectedPaths.get(0), echoDotSh);
    assertOutputSymlinkExists(expectedPaths.get(1), Paths.get("Link.java"), mainDotJava);
    assertOutputSymlinkExists(
        expectedPaths.get(2),
        Paths.get("..", "src", "com", "facebook", "buck", "Main.java"),
        mainDotJava);
    assertOutputFileExists(expectedPaths.get(3), mainDotJava);
    assertOutputFileExists(expectedPaths.get(4), otherDotJava);

    // Make sure that we set modified time and execute bit properly
    assertModifiedTime(expectedPaths);
    assertExecutable(expectedPaths.get(0), true);
    assertExecutable(expectedPaths.subList(1, expectedPaths.size()), false);
  }

  @Test
  public void cleansUpFilesThatExistInDirectoryButNotArchive() throws IOException {
    ArchiveFormat format = ArchiveFormat.TAR;

    ImmutableList<Path> expectedPaths =
        ImmutableList.of(
            getDestPath("root", "echo.sh"),
            getDestPath("root", "alternative", "Main.java"),
            getDestPath("root", "alternative", "Link.java"),
            getDestPath("root", "src", "com", "facebook", "buck", "Main.java"),
            getDestPath("root_sibling", "Other.java"));

    ImmutableList<Path> expectedDirs =
        ImmutableList.of(OUTPUT_SUBDIR, getDestPath("root"), getDestPath("root", "alternative"));

    ImmutableList<Path> junkDirs =
        ImmutableList.of(
                getDestPath("foo_bar"),
                getDestPath("root", "foo_bar"),
                getDestPath("root_sibling", "empty_dir"))
            .stream()
            .map((path) -> rootPath.resolve(path).getPath())
            .collect(ImmutableList.toImmutableList());
    ImmutableList<Path> junkFiles =
        ImmutableList.of(
                getDestPath("foo_bar", "test.file"), getDestPath("root", "foo_bar", "test.file2"))
            .stream()
            .map((path) -> rootPath.resolve(path).getPath())
            .collect(ImmutableList.toImmutableList());

    for (Path path : junkDirs) {
      Files.createDirectories(path);
    }

    for (Path path : junkFiles) {
      Files.writeString(path, "testing");
    }

    Files.createDirectories(rootPath.resolve(OUTPUT_SUBDIR).getPath());
    Files.createDirectories(rootPath.resolve(getDestPath("root")).getPath());
    Files.writeString(rootPath.resolve(getDestPath("root", "echo.sh")).getPath(), "testing");
    assertOutputFileExists(getDestPath("root", "echo.sh"), "testing");

    Path archivePath = getTestFilePath(format.getExtension());
    ImmutableList<Path> unarchivedFiles =
        format
            .getUnarchiver()
            .extractArchive(
                rootPath,
                archivePath,
                Paths.get("output_dir"),
                ExistingFileMode.OVERWRITE_AND_CLEAN_DIRECTORIES);

    assertThat(unarchivedFiles, Matchers.containsInAnyOrder(expectedPaths.toArray()));
    Assert.assertEquals(expectedPaths.size(), unarchivedFiles.size());

    assertOutputDirExists(expectedDirs.get(1));
    assertOutputDirExists(expectedDirs.get(2));
    assertModifiedTime(expectedDirs.get(2));

    // Make sure we wrote the files
    assertOutputFileExists(expectedPaths.get(0), echoDotSh);
    assertOutputSymlinkExists(expectedPaths.get(1), Paths.get("Link.java"), mainDotJava);
    assertOutputSymlinkExists(
        expectedPaths.get(2),
        Paths.get("..", "src", "com", "facebook", "buck", "Main.java"),
        mainDotJava);
    assertOutputFileExists(expectedPaths.get(3), mainDotJava);
    assertOutputFileExists(expectedPaths.get(4), otherDotJava);

    // Make sure that we set modified time and execute bit properly
    assertModifiedTime(expectedPaths);
    assertExecutable(expectedPaths.get(0), true);
    assertExecutable(expectedPaths.subList(1, expectedPaths.size()), false);
  }

  /**
   * cleanDirectoriesExactly asserts that OVERWRITE_AND_CLEAN_DIRECTORIES removes exactly the files
   * in any subdirectory of a directory entry in the tar archive.
   *
   * <p>This behavior supports unarchiving a Buck cache from multiple archives, each containing a
   * part.
   *
   * <p>Explanations:
   * <li>BUCK isn't removed because it's not in a subdirectory of a directory entry in the archive.
   * <li>buck-out/gen/pkg1/rule1.jar isn't removed, even though buck-out/gen/pkg1/rule2.jar is in
   *     the archive, because there's no directory entry for buck-out/gen/pkg1/.
   * <li>buck-out/gen/pkg1/rule2#foo/lib.so, however, is removed because the archive contains the
   *     directory buck-out/gen/pkg1/rule2#foo/
   */
  @Test
  public void cleanDirectoriesExactly() throws Exception {
    AbsPath archive = rootPath.resolve("archive.tar");
    AbsPath outDir = rootPath;

    List<String> toLeave =
        ImmutableList.of(
            "BUCK",
            "buck-out/gen/pkg1/rule1.jar",
            "buck-out/gen/pkg1/rule1#foo/lib.so",
            "buck-out/gen/pkg2/rule.jar",
            "buck-out/gen/pkg2/rule#foo/lib.so");
    List<String> toDelete = ImmutableList.of("buck-out/gen/pkg1/rule2#foo/lib.so");
    for (String s : concat(toDelete, toLeave)) {
      AbsPath path = rootPath.resolve(s);
      Files.createDirectories(path.getParent().getPath());
      Files.writeString(path.getPath(), "");
    }

    // Write test archive.
    try (TarArchiveOutputStream stream =
        new TarArchiveOutputStream(
            new BufferedOutputStream(
                new FileOutputStream(rootPath.resolve(archive.getPath()).toFile())))) {
      stream.putArchiveEntry(new TarArchiveEntry("buck-out/gen/pkg1/rule2#foo/"));
      stream.putArchiveEntry(new TarArchiveEntry("buck-out/gen/pkg1/rule2.jar"));
      stream.closeArchiveEntry();
    }

    // Untar test archive.
    Untar.tarUnarchiver()
        .extractArchive(
            outDir,
            archive.getPath(),
            outDir.getPath(),
            ExistingFileMode.OVERWRITE_AND_CLEAN_DIRECTORIES);

    // Assert expected file existence.
    for (String s : toDelete) {
      Assert.assertFalse(
          String.format("Expected file %s to be deleted, but it wasn't", s),
          Files.exists(rootPath.resolve(s).getPath()));
    }
    for (String s : toLeave) {
      Assert.assertTrue(
          String.format("Expected file %s to not be deleted, but it was", s),
          Files.exists(rootPath.resolve(s).getPath()));
    }
  }

  @Test
  public void doesNotCleanUpFilesThatExistInDirectoryButNotArchiveWithOverwriteMode()
      throws IOException {
    ArchiveFormat format = ArchiveFormat.TAR;

    ImmutableList<Path> expectedPaths =
        ImmutableList.of(
            getDestPath("root", "echo.sh"),
            getDestPath("root", "alternative", "Main.java"),
            getDestPath("root", "alternative", "Link.java"),
            getDestPath("root", "src", "com", "facebook", "buck", "Main.java"),
            getDestPath("root_sibling", "Other.java"));

    ImmutableList<Path> expectedDirs =
        ImmutableList.of(OUTPUT_SUBDIR, getDestPath("root"), getDestPath("root", "alternative"));

    ImmutableList<Path> junkDirs =
        ImmutableList.of(
                getDestPath("foo_bar"),
                getDestPath("root", "foo_bar"),
                getDestPath("root_sibling", "empty_dir"))
            .stream()
            .map((path) -> rootPath.resolve(path).getPath())
            .collect(ImmutableList.toImmutableList());
    ImmutableList<Path> junkFiles =
        ImmutableList.of(
                getDestPath("foo_bar", "test.file"), getDestPath("root", "foo_bar", "test.file2"))
            .stream()
            .map((path) -> rootPath.resolve(path).getPath())
            .collect(ImmutableList.toImmutableList());
    ;

    for (Path path : junkDirs) {
      Files.createDirectories(path);
    }

    for (Path path : junkFiles) {
      Files.writeString(path, "testing");
    }

    Files.createDirectories(rootPath.resolve(OUTPUT_SUBDIR).getPath());
    Files.createDirectories(rootPath.resolve(getDestPath("root")).getPath());
    Files.writeString(rootPath.resolve(getDestPath("root", "echo.sh")).getPath(), "testing");
    assertOutputFileExists(getDestPath("root", "echo.sh"), "testing");

    Path archivePath = getTestFilePath(format.getExtension());
    ImmutableList<Path> unarchivedFiles =
        format
            .getUnarchiver()
            .extractArchive(
                rootPath, archivePath, Paths.get("output_dir"), ExistingFileMode.OVERWRITE);

    assertThat(unarchivedFiles, Matchers.containsInAnyOrder(expectedPaths.toArray()));
    Assert.assertEquals(expectedPaths.size(), unarchivedFiles.size());

    assertOutputDirExists(expectedDirs.get(1));
    assertOutputDirExists(expectedDirs.get(2));
    assertModifiedTime(expectedDirs.get(2));

    // Make sure we wrote the files
    assertOutputFileExists(expectedPaths.get(0), echoDotSh);
    assertOutputSymlinkExists(expectedPaths.get(1), Paths.get("Link.java"), mainDotJava);
    assertOutputSymlinkExists(
        expectedPaths.get(2),
        Paths.get("..", "src", "com", "facebook", "buck", "Main.java"),
        mainDotJava);
    assertOutputFileExists(expectedPaths.get(3), mainDotJava);
    assertOutputFileExists(expectedPaths.get(4), otherDotJava);

    // Make sure that we set modified time and execute bit properly
    assertModifiedTime(expectedPaths);
    assertExecutable(expectedPaths.get(0), true);
    assertExecutable(expectedPaths.subList(1, expectedPaths.size()), false);

    Assert.assertTrue(Files.exists(junkDirs.get(0)));
    Assert.assertTrue(Files.exists(junkDirs.get(1)));
    Assert.assertTrue(Files.exists(junkDirs.get(2)));
    Assert.assertEquals("testing", Files.readAllLines(junkFiles.get(0)).get(0));
    Assert.assertEquals("testing", Files.readAllLines(junkFiles.get(1)).get(0));
  }

  @Test
  public void testExcludedEntriesNotExtracted() throws IOException {
    ArchiveFormat format = ArchiveFormat.TAR;

    ImmutableList<Path> expectedPaths =
        ImmutableList.of(
            getDestPath("echo.sh"),
            getDestPath("alternative", "Link.java"),
            getDestPath("src", "com", "facebook", "buck", "Main.java"));

    Path archivePath = getTestFilePath(format.getExtension());
    ImmutableSet<Path> unarchivedFiles =
        format
            .getUnarchiver()
            .extractArchive(
                archivePath,
                rootPath,
                Paths.get("output_dir"),
                Optional.of(Paths.get("root")),
                new PatternsMatcher(ImmutableSet.of(".*alternative/Main.java")),
                ExistingFileMode.OVERWRITE_AND_CLEAN_DIRECTORIES);

    assertThat(unarchivedFiles, Matchers.containsInAnyOrder(expectedPaths.toArray()));
    Assert.assertEquals(expectedPaths.size(), unarchivedFiles.size());
  }
}
