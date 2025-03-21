/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.testutil.integration;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.io.pathformat.PathFormatter;
import com.facebook.buck.util.MoreStringsForTests;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.io.ByteStreams;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import org.hamcrest.Matchers;

public class ZipInspector {

  private final Path zipFile;
  private final ImmutableList<String> zipFileEntries;

  public ZipInspector(Path zip) throws IOException {
    this.zipFile = Preconditions.checkNotNull(zip);

    ImmutableList.Builder<String> builder = ImmutableList.builder();
    try (ZipFile zipFile = new ZipFile(zip.toFile())) {
      Enumeration<? extends ZipEntry> entries = zipFile.entries();
      while (entries.hasMoreElements()) {
        builder.add(entries.nextElement().getName());
      }
    }
    this.zipFileEntries = builder.build();
  }

  public ZipInspector(AbsPath zip) throws IOException {
    this(zip.getPath());
  }

  public void assertFileExists(String pathRelativeToRoot) {
    assertThat(zipFileEntries, hasItem(pathRelativeToRoot));
  }

  public void assertFileDoesNotExist(String pathRelativeToRoot) {
    assertThat(zipFileEntries, not(hasItem((pathRelativeToRoot))));
  }

  public void assertFilesDoNotExist(String... pathsRelativeToRoot) {
    for (String path : pathsRelativeToRoot) {
      assertFileDoesNotExist(path);
    }
  }

  public void assertFileContents(String pathRelativeToRoot, String expected) throws IOException {
    assertFileExists(pathRelativeToRoot);
    assertThat(
        new String(getFileContents(pathRelativeToRoot), StandardCharsets.UTF_8),
        MoreStringsForTests.equalToIgnoringPlatformNewlines(expected));
  }

  public void assertFileContents(Path pathRelativeToRoot, String expected) throws IOException {
    assertFileContents(PathFormatter.pathWithUnixSeparators(pathRelativeToRoot), expected);
  }

  public void assertFileContains(String pathRelativeToRoot, String expected) throws IOException {
    assertThat(
        new String(getFileContents(pathRelativeToRoot), StandardCharsets.UTF_8),
        MoreStringsForTests.containsIgnoringPlatformNewlines(expected));
  }

  public void assertFileContains(Path pathRelativeToRoot, String expected) throws IOException {
    assertFileContains(PathFormatter.pathWithUnixSeparators(pathRelativeToRoot), expected);
  }

  public void assertFileDoesNotContain(String pathRelativeToRoot, String expected)
      throws IOException {
    assertThat(
        new String(getFileContents(pathRelativeToRoot), StandardCharsets.UTF_8),
        not(MoreStringsForTests.containsIgnoringPlatformNewlines(expected)));
  }

  public void assertFileDoesNotContain(Path pathRelativeToRoot, String expected)
      throws IOException {
    assertFileDoesNotContain(PathFormatter.pathWithUnixSeparators(pathRelativeToRoot), expected);
  }

  public byte[] getFileContents(String pathRelativeToRoot) throws IOException {
    try (ZipFile zipFile = new ZipFile(this.zipFile.toFile())) {
      ZipEntry entry = zipFile.getEntry(pathRelativeToRoot);
      if (entry == null) {
        throw new IllegalArgumentException(
            String.format(
                "%s not found in zip file %s, zip file contents [%s]",
                pathRelativeToRoot, this.zipFile, String.join(", ", zipFileEntries)));
      }
      return ByteStreams.toByteArray(zipFile.getInputStream(entry));
    }
  }

  public List<String> getFileContentsLines(String pathRelativeToRoot) throws IOException {
    return Arrays.asList(new String(getFileContents(pathRelativeToRoot)).split("\n"));
  }

  public Set<Path> getDirectoryContents(Path pathRelativeToRoot) {
    pathRelativeToRoot = pathRelativeToRoot.normalize();
    if (pathRelativeToRoot.toString().length() == 0) {
      pathRelativeToRoot = null;
    }
    Path parentPath = pathRelativeToRoot;
    return getZipFileEntries().stream()
        .map(Paths::get)
        .filter(
            path -> {
              Path pathParent = path.getParent();
              if (pathParent != null) {
                return pathParent.equals(parentPath);
              }
              return parentPath == null;
            })
        .map(Path::getFileName)
        .collect(Collectors.toSet());
  }

  public ImmutableList<String> getZipFileEntries() {
    return zipFileEntries;
  }

  public long getCrc(String pathRelativeToRoot) throws IOException {
    try (ZipFile zipFile = new ZipFile(this.zipFile.toFile())) {
      ZipEntry entry = zipFile.getEntry(pathRelativeToRoot);
      long crc = entry.getCrc();
      Preconditions.checkState(crc != -1, "Error accessing crc for entry: %s", pathRelativeToRoot);
      return crc;
    }
  }

  public long getSize(String pathRelativeToRoot) throws IOException {
    try (ZipFile zipFile = new ZipFile(this.zipFile.toFile())) {
      ZipEntry entry = zipFile.getEntry(pathRelativeToRoot);
      long size = entry.getSize();
      Preconditions.checkState(
          size != -1, "Error accessing size for entry: %s", pathRelativeToRoot);
      return size;
    }
  }

  public void assertFileIsCompressed(String pathRelativeToRoot) throws IOException {
    try (ZipFile zipFile = new ZipFile(this.zipFile.toFile())) {
      ZipEntry entry = zipFile.getEntry(pathRelativeToRoot);
      assertThat(entry.getMethod(), is(not(ZipEntry.STORED)));
      assertThat(entry.getCompressedSize(), Matchers.lessThan(entry.getSize()));
    }
  }

  public void assertFileIsNotCompressed(String pathRelativeToRoot) throws IOException {
    try (ZipFile zipFile = new ZipFile(this.zipFile.toFile())) {
      ZipEntry entry = zipFile.getEntry(pathRelativeToRoot);
      assertThat(entry.getMethod(), is(ZipEntry.STORED));
      assertThat(entry.getCompressedSize(), Matchers.equalTo(entry.getSize()));
    }
  }
}
