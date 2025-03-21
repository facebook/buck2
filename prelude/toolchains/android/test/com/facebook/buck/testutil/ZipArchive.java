/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.testutil;

import static java.nio.file.StandardOpenOption.CREATE;
import static java.nio.file.StandardOpenOption.WRITE;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.io.file.MorePaths;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Set;

/**
 * An abstraction for dealing with Zip files. Use it within a try-with-resources block for maximum
 * fun and profit. This differs from {@link java.util.zip.ZipFile} by providing a mechanism to add
 * content to the zip and by not providing a way of extracting individual entries (just the names of
 * the entries).
 */
public class ZipArchive implements AutoCloseable {

  // Java 7 introduced an abstraction for modeling file systems. One of the implementations that
  // ships with the JRE is one that handles Zip files. This allows us to work on zip files directly
  // without needing to write the contents to an intermediate directory.
  // See: http://docs.oracle.com/javase/7/docs/technotes/guides/io/fsp/zipfilesystemprovider.html
  private final FileSystem fs;
  private final Path root;

  /**
   * Open the zip file for reading and/or writing. Note that this constructor will create {@code
   * zip} if {@code forWriting} is true and the file does not exist.
   *
   * @param zip The zip file to operate on. The name must end with ".zip" or ".jar".
   * @param forWriting Whether the zip file should be opened for writing or not.
   * @throws IOException Should something terrible occur.
   */
  public ZipArchive(Path zip, boolean forWriting) throws IOException {
    String extension = MorePaths.getFileExtension(zip);
    assertTrue(
        "zip name must end with .zip for file type detection to work",
        "zip".equals(extension) || "jar".equals(extension));

    URI uri = URI.create("jar:" + zip.toUri());
    fs = FileSystems.newFileSystem(uri, ImmutableMap.of("create", String.valueOf(forWriting)));

    root = Iterables.getFirst(fs.getRootDirectories(), null);
    assertNotNull("Unable to determine root of file system: " + fs, root);
  }

  public ZipArchive(AbsPath zip, boolean forWriting) throws IOException {
    this(zip.getPath(), forWriting);
  }

  public void add(String fileName, byte[] contents) throws IOException {
    Path zipPath = fs.getPath(root.toString(), fileName);
    if (Files.notExists(zipPath.getParent())) {
      Files.createDirectories(zipPath.getParent());
    }
    Files.write(zipPath, contents, CREATE, WRITE);
  }

  public void add(String fileName, String contents) throws IOException {
    add(fileName, contents.getBytes(StandardCharsets.UTF_8));
  }

  public void addDir(String dirName) throws IOException {
    Path zipPath = fs.getPath(root.toString(), dirName);
    if (Files.notExists(zipPath)) {
      Files.createDirectories(zipPath);
    }
  }

  public Set<String> getDirNames() throws IOException {
    ImmutableSet.Builder<String> contents = ImmutableSet.builder();
    Files.walkFileTree(
        root,
        new SimpleFileVisitor<Path>() {
          @Override
          public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
            // Skip leading and trailing slashes. Java 8 returns trailing slashes, whereas Java 11
            // does not. Both return leading slashes.
            String dirName = dir.toString().substring(1);
            dirName = dirName.endsWith("/") ? dirName.substring(0, dirName.length() - 1) : dirName;
            contents.add(dirName);
            return FileVisitResult.CONTINUE;
          }
        });
    return contents.build();
  }

  public Set<String> getFileNames() throws IOException {
    ImmutableSet.Builder<String> contents = ImmutableSet.builder();
    Files.walkFileTree(
        root,
        new SimpleFileVisitor<Path>() {
          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
            // Skip the leading "/" from the path.
            contents.add(file.toString().substring(1));
            return FileVisitResult.CONTINUE;
          }
        });
    return contents.build();
  }

  public byte[] readFully(String fileName) throws IOException {
    Path resolved = root.resolve(fileName);

    return Files.readAllBytes(resolved);
  }

  @Override
  public void close() throws IOException {
    fs.close();
  }
}
