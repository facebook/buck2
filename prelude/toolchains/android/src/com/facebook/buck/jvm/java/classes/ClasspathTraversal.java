/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.classes;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.io.file.MorePaths;
import com.facebook.buck.io.file.PathMatcher;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.facebook.buck.io.pathformat.PathFormatter;
import com.facebook.buck.util.ZipFileTraversal;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileVisitOption;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Collection;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * Traversal strategy for traversing a set of paths that themselves are traversed. The provided
 * paths can point to zip/jar files, directories of resource/class files, or individual files
 * themselves.
 *
 * <p>For example, given the input paths of { foo.zip, foo/, and foo.txt }, traverse would first
 * expand foo.zip and traverse its contents, then list the files recursively in foo/, and finally
 * visit the single file foo.txt.
 */
public abstract class ClasspathTraversal {
  private final Iterable<Path> paths;
  private final AbsPath rootPath;
  private final ImmutableSet<PathMatcher> ignoredPaths;

  public ClasspathTraversal(
      Collection<Path> paths, AbsPath rootPath, ImmutableSet<PathMatcher> ignoredPaths) {
    this.paths = paths;
    this.rootPath = rootPath;
    this.ignoredPaths = ignoredPaths;
  }

  public abstract void visit(FileLike fileLike) throws IOException;

  public final void traverse() throws IOException {
    for (Path path : paths) {
      // This check is unnecessary for normal builds but when building #nullsafex or
      // #nullsafex-json flavours of java/android libraries we may run only javac frontend tasks
      // without producing any jars in the end.
      if (!ProjectFilesystemUtils.exists(rootPath, path)) {
        continue;
      }
      ClasspathTraverser adapter =
          createTraversalAdapter(ProjectFilesystemUtils.getPathForRelativePath(rootPath, path));
      adapter.traverse(this);
    }
  }

  private ClasspathTraverser createTraversalAdapter(Path path) {
    String extension = MorePaths.getFileExtension(path);
    if (ProjectFilesystemUtils.isDirectory(rootPath, path)) {
      return new DirectoryTraversalAdapter(rootPath, ignoredPaths, path);
    } else if (ProjectFilesystemUtils.isFile(rootPath, path)) {
      if (extension.equalsIgnoreCase("jar") || extension.equalsIgnoreCase("zip")) {
        return new ZipFileTraversalAdapter(path);
      } else {
        return new FileTraversalAdapter(path);
      }
    } else {
      throw new IllegalArgumentException(
          "Unsupported classpath traversal input: " + path + ". Root: " + rootPath);
    }
  }

  private static class ZipFileTraversalAdapter implements ClasspathTraverser {
    private final Path file;

    public ZipFileTraversalAdapter(Path file) {
      this.file = file;
    }

    @Override
    public void traverse(ClasspathTraversal traversal) throws IOException {
      ZipFileTraversal impl =
          new ZipFileTraversal(file) {
            @Override
            public void visit(ZipFile zipFile, ZipEntry zipEntry) throws IOException {
              traversal.visit(new FileLikeInZip(file, zipFile, zipEntry));
            }
          };
      impl.traverse();
    }

    private static class FileLikeInZip extends AbstractFileLike {
      private final Path container;
      private final ZipFile zipFile;
      private final ZipEntry entry;

      public FileLikeInZip(Path container, ZipFile zipFile, ZipEntry entry) {
        this.container = container;
        this.zipFile = zipFile;
        this.entry = entry;
      }

      @Override
      public Path getContainer() {
        return container;
      }

      @Override
      public String getRelativePath() {
        return entry.getName();
      }

      @Override
      public long getSize() {
        return entry.getSize();
      }

      @Override
      public InputStream getInput() throws IOException {
        return zipFile.getInputStream(entry);
      }
    }
  }

  private static class DirectoryTraversalAdapter implements ClasspathTraverser {
    private final AbsPath rootPath;
    private final ImmutableSet<PathMatcher> ignoredPaths;
    private final Path directory;

    public DirectoryTraversalAdapter(
        AbsPath rootPath, ImmutableSet<PathMatcher> ignoredPaths, Path directory) {
      this.rootPath = rootPath;
      this.ignoredPaths = ignoredPaths;
      this.directory = directory;
    }

    @Override
    public void traverse(ClasspathTraversal traversal) throws IOException {
      ProjectFilesystemUtils.walkFileTree(
          rootPath,
          directory,
          ImmutableSet.of(FileVisitOption.FOLLOW_LINKS),
          new SimpleFileVisitor<>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
                throws IOException {
              String relativePath =
                  PathFormatter.pathWithUnixSeparators(MorePaths.relativize(directory, file));
              traversal.visit(new FileLikeInDirectory(file, relativePath));
              return FileVisitResult.CONTINUE;
            }
          },
          ProjectFilesystemUtils.getIgnoreFilter(rootPath, true, ignoredPaths));
    }
  }

  private static class FileTraversalAdapter implements ClasspathTraverser {
    private final Path file;

    public FileTraversalAdapter(Path file) {
      this.file = file;
    }

    @Override
    public void traverse(ClasspathTraversal traversal) throws IOException {
      traversal.visit(new FileLikeInDirectory(file, file.getFileName().toString()));
    }
  }

  private static class FileLikeInDirectory extends AbstractFileLike {
    private final Path file;
    private final String relativePath;

    public FileLikeInDirectory(Path file, String relativePath) {
      // Currently, the only instances of FileLikeInDirectory appear to be the .class files
      // generated from an R.java in Android. The only exception is in unit tests.
      this.file = file;
      this.relativePath = relativePath;
    }

    @Override
    public Path getContainer() {
      return file;
    }

    @Override
    public String getRelativePath() {
      return relativePath;
    }

    @Override
    public long getSize() throws IOException {
      return Files.size(file);
    }

    @Override
    public InputStream getInput() throws IOException {
      return Files.newInputStream(file);
    }
  }
}
