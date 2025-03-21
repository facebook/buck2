/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.io.file;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.io.windowsfs.WindowsFS;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.CharMatcher;
import com.google.common.base.Functions;
import com.google.common.base.Objects;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableSet;
import com.google.common.io.ByteStreams;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.nio.file.attribute.PosixFilePermission;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class MostFiles {

  private static final Logger LOG = Logger.get(MostFiles.class);

  // Extended attribute bits for directories and symlinks; see:
  // http://unix.stackexchange.com/questions/14705/the-zip-formats-external-file-attribute
  @SuppressWarnings("PMD.AvoidUsingOctalValues")
  public static final long S_IFDIR = 0040000;

  @SuppressWarnings("PMD.AvoidUsingOctalValues")
  public static final long S_IFREG = 0100000;

  @SuppressWarnings("PMD.AvoidUsingOctalValues")
  public static final long S_IFLNK = 0120000;

  public enum DeleteRecursivelyOptions {
    IGNORE_NO_SUCH_FILE_EXCEPTION,
    DELETE_CONTENTS_ONLY,
  }

  // Unix has two illegal characters - '/', and '\0'.  Windows has ten, which includes those two.
  // The full list can be found at https://msdn.microsoft.com/en-us/library/aa365247
  private static final String ILLEGAL_FILE_NAME_CHARACTERS = "<>:\"/\\|?*\0";

  private static final WindowsFS winFS = new WindowsFS();

  private static class FileAccessedEntry {
    public final File file;
    public final FileTime lastAccessTime;

    public File getFile() {
      return file;
    }

    public FileTime getLastAccessTime() {
      return lastAccessTime;
    }

    private FileAccessedEntry(File file, FileTime lastAccessTime) {
      this.file = file;
      this.lastAccessTime = lastAccessTime;
    }
  }

  /** Sorts by the lastAccessTime in descending order (more recently accessed files are first). */
  private static final Comparator<FileAccessedEntry> SORT_BY_LAST_ACCESSED_TIME_DESC =
      (a, b) -> b.getLastAccessTime().compareTo(a.getLastAccessTime());

  /** Utility class: do not instantiate. */
  private MostFiles() {}

  /**
   * Deletes the file tree rooted at {@code path} while excluding any paths from {@code
   * excludedAbsPaths}. If any files are not deleted due to presence in {@code excludedAbsPaths},
   * then any parent directories will not be deleted, either.
   */
  public static void deleteRecursivelyIfExists(AbsPath path, ImmutableSet<AbsPath> excludedAbsPaths)
      throws IOException {
    ImmutableSet<Path> excludedPaths =
        excludedAbsPaths.stream()
            .map(absPath -> absPath.getPath())
            .collect(ImmutableSet.toImmutableSet());
    deleteRecursivelyWithOptions(
        path.getPath(),
        EnumSet.of(DeleteRecursivelyOptions.IGNORE_NO_SUCH_FILE_EXCEPTION),
        excludedPaths);
  }

  public static void deleteRecursivelyIfExists(Path path) throws IOException {
    deleteRecursivelyWithOptions(
        path, EnumSet.of(DeleteRecursivelyOptions.IGNORE_NO_SUCH_FILE_EXCEPTION));
  }

  public static void deleteRecursivelyIfExists(AbsPath path) throws IOException {
    deleteRecursivelyIfExists(path.getPath());
  }

  /** Recursively copies all files under {@code fromPath} to {@code toPath}. */
  public static void copyRecursively(Path fromPath, Path toPath) throws IOException {
    copyRecursively(fromPath, toPath, Functions.identity());
  }

  /** Recursively copies all files under {@code fromPath} to {@code toPath}. */
  public static void copyRecursively(AbsPath fromPath, AbsPath toPath) throws IOException {
    copyRecursively(fromPath.getPath(), toPath.getPath());
  }

  /**
   * Recursively copies all files under {@code fromPath} to {@code toPath}. The {@code transform}
   * will be applied after the destination path for a file has been relativized. This will remove
   * any existing files in the toPath if there is a conflict.
   *
   * @param fromPath item to copy
   * @param toPath destination of copy
   * @param transform renaming function to apply when copying. If this function returns null, then
   *     the file is not copied.
   */
  public static void copyRecursively(Path fromPath, Path toPath, Function<Path, Path> transform)
      throws IOException {
    copyRecursively(fromPath, toPath, transform, input -> true);
  }

  public static void copyRecursively(
      Path fromPath, Path toPath, Function<Path, Path> transform, Function<Path, Boolean> filter)
      throws IOException {
    // Adapted from http://codingjunkie.net/java-7-copy-move/.
    SimpleFileVisitor<Path> copyDirVisitor =
        new SimpleFileVisitor<Path>() {

          @Override
          public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs)
              throws IOException {
            Path targetPath = toPath.resolve(fromPath.relativize(dir));
            if (!Files.exists(targetPath)) {
              Files.createDirectory(targetPath);
            }
            return FileVisitResult.CONTINUE;
          }

          @Override
          public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
              throws IOException {
            if (!filter.apply(file)) {
              return FileVisitResult.CONTINUE;
            }
            Path destPath = toPath.resolve(fromPath.relativize(file));
            Path transformedDestPath = transform.apply(destPath);
            if (transformedDestPath != null) {
              if (Files.isSymbolicLink(file)) {
                Files.deleteIfExists(transformedDestPath);
                MorePaths.createSymLink(winFS, transformedDestPath, Files.readSymbolicLink(file));
              } else {
                Files.copy(file, transformedDestPath, StandardCopyOption.REPLACE_EXISTING);
              }
            }
            return FileVisitResult.CONTINUE;
          }
        };
    Files.walkFileTree(fromPath, copyDirVisitor);
  }

  public static void deleteRecursively(Path path) throws IOException {
    deleteRecursivelyWithOptions(path, EnumSet.noneOf(DeleteRecursivelyOptions.class));
  }

  public static void deleteRecursively(AbsPath path) throws IOException {
    deleteRecursively(path.getPath());
  }

  public static void deleteRecursivelyWithOptions(
      Path path, EnumSet<DeleteRecursivelyOptions> options) throws IOException {
    deleteRecursivelyWithOptions(path, options, ImmutableSet.of());
  }

  /**
   * Returns {@code true} if the given {@code path} or any of its ancestors are contained in {@code
   * excludedPaths}. For example, if {@code excludedPaths} contains "/a/b" and and {@code path} is
   * "/a/b/c/d", then this function will return {@code true}. On the other hand, if {@code path} is
   * "/a/f/g, then the function returns {@code false}.
   */
  public static boolean shouldSkipDeletingPath(Path path, ImmutableSet<Path> excludedPaths) {
    Preconditions.checkArgument(path.isAbsolute());

    if (excludedPaths.contains(path)) {
      return true;
    }

    for (Path excludedPath : excludedPaths) {
      // Required so that `excludedPaths` can contain directories
      if (path.startsWith(excludedPath)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Deletes the file element rooted at {@code path}, recursively if a directory, by excluding any
   * elements from {@code excludedPaths}. If {@code excludedPaths} contains directories, then all
   * elements contain in those directories are excluded as well.
   */
  public static void deleteRecursivelyWithOptions(
      Path path, EnumSet<DeleteRecursivelyOptions> options, ImmutableSet<Path> excludedPaths)
      throws IOException {
    try {
      if (!Files.isDirectory(path)) {
        if (options.contains(DeleteRecursivelyOptions.IGNORE_NO_SUCH_FILE_EXCEPTION)) {
          if (!Files.exists(path, LinkOption.NOFOLLOW_LINKS)) {
            return;
          }
        }
        if (options.contains(DeleteRecursivelyOptions.DELETE_CONTENTS_ONLY)) {
          throw new IOException(String.format("Can't delete contents of regular file %s.", path));
        }
        Preconditions.checkArgument(excludedPaths.isEmpty());
        Files.delete(path);
      } else {
        // `nonEmptyDirectories` is a list of directories which contains elements (files/dirs)
        // that were excluded from deletion, so these parent directories must be kept. Note that
        // semantics of `nonEmptyDirectories` are different from `excludedPaths`, namely:
        // `excludedPaths` are _recursive_ while `nonEmptyDirectories` are _not_.
        //
        // That is, a directory in `nonEmptyDirectories` does not mean that _all_ elements
        // inside that directories must be kept but only the directory _itself_.
        Set<Path> nonEmptyDirectories = new HashSet<>();
        Consumer<Path> skipDeletingParentsOf =
            (Path excludedPath) -> {
              Path currentParent = excludedPath.getParent();
              while (currentParent != null) {
                nonEmptyDirectories.add(currentParent);
                currentParent = currentParent.getParent();
              }
            };

        // Adapted from http://codingjunkie.net/java-7-copy-move/.
        SimpleFileVisitor<Path> deleteDirVisitor =
            new SimpleFileVisitor<Path>() {

              @Override
              public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
                  throws IOException {
                try {
                  if (!shouldSkipDeletingPath(file.toAbsolutePath(), excludedPaths)) {
                    Files.delete(file);
                  } else {
                    skipDeletingParentsOf.accept(file);
                  }
                } catch (IOException e) {
                  LOG.warn("%s, could not delete file", e);
                }
                return FileVisitResult.CONTINUE;
              }

              @Override
              public FileVisitResult postVisitDirectory(Path dir, IOException e)
                  throws IOException {
                if (e == null) {
                  // Allow leaving the top-level directory in place (e.g. for deleting the contents
                  // of
                  // the trash dir but not the trash dir itself)
                  if (!(options.contains(DeleteRecursivelyOptions.DELETE_CONTENTS_ONLY)
                          && dir.equals(path))
                      && !nonEmptyDirectories.contains(dir)) {
                    try {
                      if (!shouldSkipDeletingPath(dir.toAbsolutePath(), excludedPaths)) {
                        Files.delete(dir);
                      } else {
                        skipDeletingParentsOf.accept(dir);
                      }
                    } catch (DirectoryNotEmptyException notEmpty) {
                      try (Stream<Path> paths = Files.list(dir)) {
                        LOG.warn(
                            String.format(
                                "Could not delete non-empty directory %s. Contents:\n%s",
                                dir, paths.map(Path::toString).collect(Collectors.joining("\n"))));
                      }
                    } catch (IOException ioException) {
                      LOG.warn("%s, could not delete directory", ioException);
                    }
                  }
                  return FileVisitResult.CONTINUE;
                } else {
                  throw e;
                }
              }
            };
        Files.walkFileTree(path, deleteDirVisitor);
      }
    } catch (NoSuchFileException e) {
      if (!options.contains(DeleteRecursivelyOptions.IGNORE_NO_SUCH_FILE_EXCEPTION)) {
        throw e;
      }
    }
  }

  /** Writes the specified lines to the specified file, encoded as UTF-8. */
  public static void writeLinesToFile(Iterable<String> lines, Path file) throws IOException {
    try (BufferedWriter writer = Files.newBufferedWriter(file, StandardCharsets.UTF_8)) {
      for (String line : lines) {
        writer.write(line);
        writer.newLine();
      }
    }
  }

  /** Writes the specified lines to the specified file, encoded as UTF-8. */
  public static void writeLinesToFile(Iterable<String> lines, AbsPath file) throws IOException {
    writeLinesToFile(lines, file.getPath());
  }

  /** Log a simplistic diff between lines and the contents of file. */
  @VisibleForTesting
  static List<String> diffFileContents(Iterable<String> lines, File file) throws IOException {
    List<String> diffLines = new ArrayList<>();
    Iterator<String> iter = lines.iterator();
    try (BufferedReader reader = Files.newBufferedReader(file.toPath(), StandardCharsets.UTF_8)) {
      while (iter.hasNext()) {
        String lineA = reader.readLine();
        String lineB = iter.next();
        if (!Objects.equal(lineA, lineB)) {
          diffLines.add(String.format("| %s | %s |", lineA == null ? "" : lineA, lineB));
        }
      }

      String lineA;
      while ((lineA = reader.readLine()) != null) {
        diffLines.add(String.format("| %s |  |", lineA));
      }
    }
    return diffLines;
  }

  /**
   * Does an in-place sort of the specified {@code files} array. Most recently accessed files will
   * be at the front of the array when sorted.
   */
  public static void sortFilesByAccessTime(File[] files) {
    FileAccessedEntry[] fileAccessedEntries = new FileAccessedEntry[files.length];
    for (int i = 0; i < files.length; ++i) {
      FileTime lastAccess;
      try {
        lastAccess =
            Files.readAttributes(files[i].toPath(), BasicFileAttributes.class).lastAccessTime();
      } catch (IOException e) {
        lastAccess = FileTime.fromMillis(files[i].lastModified());
      }
      fileAccessedEntries[i] = new FileAccessedEntry(files[i], lastAccess);
    }
    Arrays.sort(fileAccessedEntries, SORT_BY_LAST_ACCESSED_TIME_DESC);

    for (int i = 0; i < files.length; i++) {
      files[i] = fileAccessedEntries[i].getFile();
    }
  }

  /**
   * Tries to make the specified file executable. For file systems that do support the POSIX-style
   * permissions, the executable permission is set for each category of users that already has the
   * read permission.
   *
   * <p>If the file system does not support the executable permission or the operation fails, a
   * {@code java.io.IOException} is thrown.
   */
  public static void makeExecutable(Path file) throws IOException {
    if (FileSystems.getDefault().supportedFileAttributeViews().contains("posix")) {
      Set<PosixFilePermission> permissions = Files.getPosixFilePermissions(file);

      if (permissions.contains(PosixFilePermission.OWNER_READ)) {
        permissions.add(PosixFilePermission.OWNER_EXECUTE);
      }
      if (permissions.contains(PosixFilePermission.GROUP_READ)) {
        permissions.add(PosixFilePermission.GROUP_EXECUTE);
      }
      if (permissions.contains(PosixFilePermission.OTHERS_READ)) {
        permissions.add(PosixFilePermission.OTHERS_EXECUTE);
      }

      Files.setPosixFilePermissions(file, permissions);
    } else {
      if (!file.toFile().setExecutable(/* executable */ true, /* ownerOnly */ true)) {
        throw new IOException("The file could not be made executable");
      }
    }
  }

  /**
   * Tries to make the specified file writable. For file systems that do support the POSIX-style
   * permissions, the writable permission is set for each category of users that already has the
   * read permission.
   *
   * <p>If the file system does not support the writable permission or the operation fails, a {@code
   * java.io.IOException} is thrown.
   */
  public static void makeWritable(Path file) throws IOException {
    if (FileSystems.getDefault().supportedFileAttributeViews().contains("posix")) {
      Set<PosixFilePermission> permissions = Files.getPosixFilePermissions(file);

      if (permissions.contains(PosixFilePermission.OWNER_READ)) {
        permissions.add(PosixFilePermission.OWNER_WRITE);
      }
      if (permissions.contains(PosixFilePermission.GROUP_READ)) {
        permissions.add(PosixFilePermission.GROUP_WRITE);
      }
      if (permissions.contains(PosixFilePermission.OTHERS_READ)) {
        permissions.add(PosixFilePermission.OTHERS_WRITE);
      }

      Files.setPosixFilePermissions(file, permissions);
    } else {
      if (!file.toFile().setWritable(/* writable */ true, /* ownerOnly */ true)) {
        throw new IOException("The file could not be made writable");
      }
    }
  }

  /** Abs-path version of {@link #makeExecutable(Path)}. */
  public static void makeExecutable(AbsPath file) throws IOException {
    makeExecutable(file.getPath());
  }

  /**
   * Given a file name, replace any illegal characters from it.
   *
   * @param name The file name to sanitize
   * @return a properly sanitized filename
   */
  public static String sanitize(String name) {
    return CharMatcher.anyOf(ILLEGAL_FILE_NAME_CHARACTERS).replaceFrom(name, "_");
  }

  /**
   * Concatenates the contents of one or more files.
   *
   * @param dest The path to which the concatenated files' contents are written.
   * @param pathsToConcatenate The paths whose contents are concatenated to {@code dest}.
   * @return {@code true} if any data was concatenated to {@code dest}, {@code false} otherwise.
   */
  public static boolean concatenateFiles(Path dest, Iterable<Path> pathsToConcatenate)
      throws IOException {
    // Concatenate all the logs to a temp file, then atomically rename it to the
    // passed-in concatenatedPath if any log data was collected.
    String tempFilename = "." + dest.getFileName() + ".concat.tmp." + UUID.randomUUID();
    Path tempPath = dest.resolveSibling(tempFilename);

    try {
      long bytesCollected = 0;
      try (OutputStream os =
          Files.newOutputStream(
              tempPath,
              StandardOpenOption.CREATE,
              StandardOpenOption.TRUNCATE_EXISTING,
              StandardOpenOption.WRITE)) {
        for (Path path : pathsToConcatenate) {
          try (InputStream is = Files.newInputStream(path)) {
            bytesCollected += ByteStreams.copy(is, os);
          } catch (NoSuchFileException e) {
            continue;
          }
        }
      }
      if (bytesCollected > 0) {
        Files.move(
            tempPath, dest, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
        return true;
      } else {
        return false;
      }
    } finally {
      Files.deleteIfExists(tempPath);
    }
  }

  /** Write given string to the file using UTF-8 charset. */
  public static void write(AbsPath path, String content) throws IOException {
    Files.write(path.getPath(), content.getBytes(StandardCharsets.UTF_8));
  }
}
