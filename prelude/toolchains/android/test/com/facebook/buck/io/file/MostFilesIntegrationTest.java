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
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.testutil.TemporaryPaths;
import com.facebook.buck.util.CreateSymlinksForTests;
import com.facebook.buck.util.environment.Platform;
import com.google.common.base.Joiner;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.junit.Assume;
import org.junit.Rule;
import org.junit.Test;

public class MostFilesIntegrationTest {

  @Rule public TemporaryPaths tmp = new TemporaryPaths();

  @Test
  public void testCopyTestdataDirectoryWithSymlinks() throws IOException {
    Platform platform = Platform.detect();
    Assume.assumeTrue(platform == Platform.LINUX || platform == Platform.MACOS);

    AbsPath root = tmp.newFolder();

    AbsPath srcDir = root.resolve("src");
    Files.createDirectory(srcDir.getPath());
    AbsPath sourceFile = srcDir.resolve("file.txt");
    Files.write(sourceFile.getPath(), "contents\n".getBytes(UTF_8));

    CreateSymlinksForTests.createSymLink(srcDir.resolve("link.txt"), srcDir.relativize(sourceFile));

    MostFiles.copyRecursively(root.resolve("src"), root.resolve("out"));
    assertTrue(Files.isSymbolicLink(root.resolve("src/link.txt").getPath()));
    assertTrue(Files.isSymbolicLink(root.resolve("out/link.txt").getPath()));

    byte[] bytes = Files.readAllBytes(root.resolve("out/link.txt").getPath());
    assertArrayEquals("contents\n".getBytes(), bytes);

    assertEquals(
        "link.txt should point to file.txt in the same directory.",
        Paths.get("file.txt"),
        Files.readSymbolicLink(root.resolve("out/link.txt").getPath()));

    Files.write(root.resolve("src/link.txt").getPath(), "replacement\n".getBytes());

    assertArrayEquals(
        "replacement\n".getBytes(), Files.readAllBytes(root.resolve("src/file.txt").getPath()));
    assertArrayEquals(
        "The replacement bytes should be reflected in the symlink.",
        "replacement\n".getBytes(),
        Files.readAllBytes(root.resolve("src/link.txt").getPath()));
    assertArrayEquals(
        "contents\n".getBytes(), Files.readAllBytes(root.resolve("out/file.txt").getPath()));
    assertArrayEquals(
        "The copied symlink should be unaffected.",
        "contents\n".getBytes(),
        Files.readAllBytes(root.resolve("out/link.txt").getPath()));
  }

  @Test
  public void testDiffFileContents() throws IOException {
    AbsPath inputFile = tmp.newFolder().resolve("MostFiles.txt");
    Files.write(inputFile.getPath(), Joiner.on("\n").join("AAA", "BBB", "CCC").getBytes(UTF_8));

    List<String> diffLines;
    String testPath = inputFile.toString();
    File testFile = new File(testPath);

    diffLines = MostFiles.diffFileContents(Arrays.asList("AAA", "BBB", "CCC"), testFile);
    assertEquals(diffLines, new ArrayList<String>());

    diffLines = MostFiles.diffFileContents(Arrays.asList("AAA", "BBB"), testFile);
    assertEquals(diffLines, Collections.singletonList("| CCC |  |"));

    diffLines = MostFiles.diffFileContents(Arrays.asList("AAA", "CCC"), testFile);
    assertEquals(diffLines, Arrays.asList("| BBB | CCC |", "| CCC |  |"));

    diffLines = MostFiles.diffFileContents(Arrays.asList("BBB", "CCC"), testFile);
    assertEquals(diffLines, Arrays.asList("| AAA | BBB |", "| BBB | CCC |", "| CCC |  |"));

    diffLines = MostFiles.diffFileContents(Collections.singletonList("AAA"), testFile);
    assertEquals(diffLines, Arrays.asList("| BBB |  |", "| CCC |  |"));

    diffLines = MostFiles.diffFileContents(Collections.singletonList("BBB"), testFile);
    assertEquals(diffLines, Arrays.asList("| AAA | BBB |", "| BBB |  |", "| CCC |  |"));

    diffLines = MostFiles.diffFileContents(Collections.singletonList("CCC"), testFile);
    assertEquals(diffLines, Arrays.asList("| AAA | CCC |", "| BBB |  |", "| CCC |  |"));

    diffLines = MostFiles.diffFileContents(new ArrayList<>(), testFile);
    assertEquals(diffLines, Arrays.asList("| AAA |  |", "| BBB |  |", "| CCC |  |"));

    diffLines = MostFiles.diffFileContents(Arrays.asList("AAA", "BBB", "CCC", "xxx"), testFile);
    assertEquals(diffLines, Collections.singletonList("|  | xxx |"));

    diffLines = MostFiles.diffFileContents(Arrays.asList("AAA", "BBB", "xxx", "CCC"), testFile);
    assertEquals(diffLines, Arrays.asList("| CCC | xxx |", "|  | CCC |"));

    diffLines = MostFiles.diffFileContents(Arrays.asList("AAA", "xxx", "BBB", "CCC"), testFile);
    assertEquals(diffLines, Arrays.asList("| BBB | xxx |", "| CCC | BBB |", "|  | CCC |"));

    diffLines = MostFiles.diffFileContents(Arrays.asList("xxx", "AAA", "BBB", "CCC"), testFile);
    assertEquals(
        diffLines, Arrays.asList("| AAA | xxx |", "| BBB | AAA |", "| CCC | BBB |", "|  | CCC |"));

    diffLines = MostFiles.diffFileContents(Arrays.asList("AAA", "BBB", "xxx"), testFile);
    assertEquals(diffLines, Collections.singletonList("| CCC | xxx |"));

    diffLines = MostFiles.diffFileContents(Arrays.asList("AAA", "xxx", "CCC"), testFile);
    assertEquals(diffLines, Collections.singletonList("| BBB | xxx |"));

    diffLines = MostFiles.diffFileContents(Arrays.asList("xxx", "BBB", "CCC"), testFile);
    assertEquals(diffLines, Collections.singletonList("| AAA | xxx |"));

    diffLines = MostFiles.diffFileContents(Arrays.asList("AAA", "xxx", "yyy"), testFile);
    assertEquals(diffLines, Arrays.asList("| BBB | xxx |", "| CCC | yyy |"));

    diffLines = MostFiles.diffFileContents(Arrays.asList("xxx", "BBB", "yyy"), testFile);
    assertEquals(diffLines, Arrays.asList("| AAA | xxx |", "| CCC | yyy |"));

    diffLines = MostFiles.diffFileContents(Arrays.asList("xxx", "yyy", "CCC"), testFile);
    assertEquals(diffLines, Arrays.asList("| AAA | xxx |", "| BBB | yyy |"));

    diffLines = MostFiles.diffFileContents(Arrays.asList("xxx", "yyy", "zzz"), testFile);
    assertEquals(diffLines, Arrays.asList("| AAA | xxx |", "| BBB | yyy |", "| CCC | zzz |"));
  }
}
