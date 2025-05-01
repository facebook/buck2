/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.io.file;

import com.facebook.buck.core.exceptions.HumanReadableException;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.PathWrapper;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.io.pathformat.PathFormatter;
import com.facebook.buck.io.windowsfs.WindowsFS;
import com.facebook.buck.util.environment.Platform;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Streams;
import com.google.common.io.ByteSource;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileSystem;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import javax.annotation.Nullable;

/** Common functions that are done with a {@link Path}. */
public class MorePaths {

  private static final boolean IS_WINDOWS = Platform.detect() == Platform.WINDOWS;

  @Nullable public static final WindowsFS WIN_FS_INSTANCE = IS_WINDOWS ? new WindowsFS() : null;

  /** Utility class: do not instantiate. */
  private MorePaths() {}

  public static String pathWithPlatformSeparators(String path) {
    return pathWithPlatformSeparators(Paths.get(path));
  }

  public static String pathWithPlatformSeparators(Path path) {
    if (IS_WINDOWS) {
      return PathFormatter.pathWithWindowsSeparators(path);
    } else {
      return PathFormatter.pathWithUnixSeparators(path);
    }
  }

  public static String pathWithPlatformSeparators(PathWrapper path) {
    return pathWithPlatformSeparators(path.getPath());
  }

  public static Path getParentOrEmpty(Path path) {
    Path parent = path.getParent();
    if (parent == null) {
      parent = emptyOf(path);
    }
    return parent;
  }

  public static RelPath getParentOrEmpty(RelPath path) {
    return RelPath.of(getParentOrEmpty(path.getPath()));
  }

  public static RelPath appendSuffix(RelPath path, String suffix) {
    return RelPath.of(getParentOrEmpty(path).resolve(path.getFileName() + suffix));
  }

  /**
   * Get the path of a file relative to a base directory.
   *
   * @param path must reference a file, not a directory.
   * @param baseDir must reference a directory that is relative to a common directory with the path.
   *     may be null if referencing the same directory as the path.
   * @return the relative path of path from the directory baseDir.
   */
  public static RelPath getRelativePath(RelPath path, @Nullable RelPath baseDir) {
    if (baseDir == null) {
      // This allows callers to use this method with "file.parent()" for files from the project
      // root dir.
      baseDir = RelPath.of(emptyOf(path.getPath()));
    }
    return relativize(baseDir, path);
  }

  /**
   * Get a relative path from path1 to path2, first normalizing each path.
   *
   * <p>This method is a workaround for JDK-6925169 (Path.relativize returns incorrect result if
   * path contains "." or "..").
   */
  public static Path relativize(Path path1, Path path2) {
    assertSamePathType(path1, path2);

    path1 = normalize(path1);
    path2 = normalize(path2);

    // On Windows, if path1 is "" then Path.relativize returns ../path2 instead of path2 or ./path2
    if (isEmpty(path1)) {
      return path2;
    }
    return path1.relativize(path2);
  }

  private static String getPathTypeString(Path path) {
    return path.isAbsolute() ? "absolute" : "relative";
  }

  public static RelPath relativize(PathWrapper path1, PathWrapper path2) {
    return RelPath.of(relativize(path1.getPath(), path2.getPath()));
  }

  /**
   * Returns a child path relative to a base path. This is similar to `Path.relativize`, but
   * supports base paths that start with "..", even in Java 11. JCL implementations of
   * `Path.relativize` support base paths like this in Java 8, but not in Java 11.
   *
   * @param basePath the path against which childPath will be relativized
   * @param childPath the path to relativize against {@code basePath}
   * @return {@code childPath} relativized against {@code basePath}
   */
  public static Path relativizeWithDotDotSupport(Path basePath, Path childPath) {
    FileSystem fileSystem = basePath.getFileSystem();
    if (basePath.equals(childPath)) {
      return fileSystem.getPath("");
    }

    assertSamePathType(basePath, childPath);

    // Skip past equal prefixes.
    int idx = 0;
    while (idx < basePath.getNameCount()
        && idx < childPath.getNameCount()
        && basePath.getName(idx).equals(childPath.getName(idx))) {
      idx++;
    }

    // Add ".."s to get to the root of the remainder of the base path.
    StringBuilder result = new StringBuilder();
    for (int i = idx; i < basePath.getNameCount(); i++) {
      if (!basePath.getName(i).toString().isEmpty()) {
        result.append("..");
        result.append(File.separatorChar);
      }
    }

    // Now add the remainder of the child path.
    if (idx < childPath.getNameCount()) {
      result.append(childPath.getName(idx).toString());
    }
    for (int i = idx + 1; i < childPath.getNameCount(); i++) {
      result.append(File.separatorChar);
      result.append(childPath.getName(i).toString());
    }

    return fileSystem.getPath(result.toString());
  }

  private static void assertSamePathType(Path basePath, Path childPath) {
    Preconditions.checkArgument(
        basePath.isAbsolute() == childPath.isAbsolute(),
        "Both paths must be absolute or both paths must be relative. (%s is %s, %s is %s)",
        basePath,
        getPathTypeString(basePath),
        childPath,
        getPathTypeString(childPath));
  }

  /**
   * Get a path without unnecessary path parts.
   *
   * <p>This method is a workaround for JDK-8037945 (Paths.get("").normalize() throws
   * ArrayIndexOutOfBoundsException).
   */
  public static Path normalize(Path path) {
    if (!isEmpty(path)) {
      path = path.normalize();
    }
    return path;
  }

  /** Type-safer version of {@link #normalize(Path)}. */
  public static AbsPath normalize(AbsPath path) {
    return AbsPath.of(normalize(path.getPath()));
  }

  /** Return empty path with the same filesystem as provided path */
  public static Path emptyOf(Path path) {
    return path.getFileSystem().getPath("");
  }

  /** Return true if provided path is empty path ("") */
  public static boolean isEmpty(Path path) {
    return emptyOf(path).equals(path);
  }

  /**
   * Filters out {@link Path} objects from {@code paths} that aren't a subpath of {@code root} and
   * returns a set of paths relative to {@code root}.
   */
  public static ImmutableSet<Path> filterForSubpaths(Iterable<Path> paths, Path root) {
    Path normalizedRoot = root.toAbsolutePath().normalize();
    return Streams.stream(paths)
        .filter(
            input -> {
              if (input.isAbsolute()) {
                return input.normalize().startsWith(normalizedRoot);
              } else {
                return true;
              }
            })
        .map(
            input -> {
              if (input.isAbsolute()) {
                return relativize(normalizedRoot, input);
              } else {
                return input;
              }
            })
        .collect(ImmutableSet.toImmutableSet());
  }

  /** Expands "~/foo" into "/home/zuck/foo". Returns regular paths unmodified. */
  public static Path expandHomeDir(Path path) {
    if (!path.startsWith("~")) {
      return path;
    }
    Path homePath = path.getFileSystem().getPath(System.getProperty("user.home"));
    if (path.equals(path.getFileSystem().getPath("~"))) {
      return homePath;
    }
    return homePath.resolve(path.subpath(1, path.getNameCount()));
  }

  public static ByteSource asByteSource(Path path) {
    return new ByteSource() {
      @Override
      public InputStream openStream() throws IOException {
        return Files.newInputStream(path);
      }
    };
  }

  public static String getFileExtension(Path path) {
    String name = path.getFileName().toString();
    int index = name.lastIndexOf('.');
    return index == -1 ? "" : name.substring(index + 1);
  }

  public static String getFileExtension(PathWrapper path) {
    return getFileExtension(path.getPath());
  }

  public static String getNameWithoutExtension(Path file) {
    String name = file.getFileName().toString();
    int index = name.lastIndexOf('.');
    return index == -1 ? name : name.substring(0, index);
  }

  public static String getNameWithoutExtension(PathWrapper file) {
    return getNameWithoutExtension(file.getPath());
  }

  public static String stripPathPrefixAndExtension(Path fileName, String prefix) {
    String nameWithoutExtension = getNameWithoutExtension(fileName);

    if (!nameWithoutExtension.startsWith(prefix)
        || nameWithoutExtension.length() < prefix.length()) {
      throw new HumanReadableException(
          "Invalid prefix on filename in path %s (file %s) - expecting %s",
          fileName, nameWithoutExtension, prefix);
    }

    return nameWithoutExtension.substring(prefix.length());
  }

  /**
   * @return strip {@code suffix} from the end of {@code p} and return the result, or {@code
   *     Optional.empty()} if {@code p} does not end with {@code suffix}.
   */
  public static Optional<Path> stripSuffix(Path p, Path suffix) {
    if (suffix.getNameCount() > p.getNameCount()) {
      return Optional.empty();
    }
    for (int i = 1; i <= suffix.getNameCount(); ++i) {
      if (!suffix.getName(suffix.getNameCount() - i).equals(p.getName(p.getNameCount() - i))) {
        return Optional.empty();
      }
    }
    if (p.getNameCount() == suffix.getNameCount()) {
      return Optional.of(p.getFileSystem().getPath(""));
    }
    return Optional.of(p.subpath(0, p.getNameCount() - suffix.getNameCount()));
  }

  public static Optional<RelPath> stripSuffix(RelPath p, RelPath suffix) {
    return stripSuffix(p.getPath(), suffix.getPath()).map(RelPath::of);
  }

  public static Optional<Path> stripPrefix(Path p, Path prefix) {
    if (prefix.getNameCount() > p.getNameCount()) {
      return Optional.empty();
    }
    for (int i = 0; i < prefix.getNameCount(); ++i) {
      if (!prefix.getName(i).equals(p.getName(i))) {
        return Optional.empty();
      }
    }
    return Optional.of(p.subpath(prefix.getNameCount(), p.getNameCount()));
  }

  public static Optional<RelPath> stripPrefix(RelPath p, RelPath prefix) {
    return stripPrefix(p.getPath(), prefix.getPath()).map(RelPath::of);
  }

  public static Function<String, Path> toPathFn(FileSystem fileSystem) {
    return input -> fileSystem.getPath(input);
  }

  private static Path dropPathPart(Path p, int i) {
    if (i == 0) {
      return p.subpath(1, p.getNameCount());
    } else if (i == p.getNameCount() - 1) {
      return p.subpath(0, p.getNameCount() - 1);
    } else {
      return p.subpath(0, i).resolve(p.subpath(i + 1, p.getNameCount()));
    }
  }

  /**
   * Drop any "." parts (useless). Do keep ".." parts; don't normalize them away.
   *
   * <p>Note that while Path objects provide a {@link Path#normalize()} method for eliminating
   * redundant parts of paths like in {@code foo/a/../b/c}, changing its internal parts (and
   * actually using the filesystem), we don't use those methods to clean up the incoming paths; we
   * only strip empty parts, and those consisting only of {@code .} because doing so maps
   * exactly-same paths together, and can't influence where it may point to, whereas {@code ..} and
   * symbolic links might.
   */
  public static Path fixPath(Path p) {
    int i = 0;
    while (i < p.getNameCount()) {
      if (p.getName(i).toString().equals(".")) {
        p = dropPathPart(p, i);
      } else {
        i++;
      }
    }
    return p;
  }

  /**
   * Drop the cache in Path object.
   *
   * <p>Path's implementation class {@code UnixPath}, will lazily initialize a String representation
   * and store it in the object when {@code #toString()} is called for the first time. This doubles
   * the memory requirement for the Path object.
   *
   * <p>This hack constructs a new path, dropping the cached toString value.
   *
   * <p>Due to the nature of what this function does, it's very sensitive to the implementation. Any
   * calls to {@code #toString()} on the returned object would also recreate the cached string
   * value.
   */
  public static Path dropInternalCaches(Path p) {
    return p.getFileSystem().getPath(p.toString());
  }

  /**
   * Creates a symlink.
   *
   * @param winFS WindowsFS object that creates symlink on Windows using different permission level
   *     implementations.
   * @param symLink the symlink to create.
   * @param target the target of the symlink.
   * @throws IOException if an I/O error occurs
   */
  public static void createSymLink(@Nullable WindowsFS winFS, Path symLink, Path target)
      throws IOException {
    try {
      if (IS_WINDOWS) {
        Objects.requireNonNull(winFS);
        target = MorePaths.normalize(symLink.getParent().resolve(target));
        // `isDirectory()` here returns false if target doesn’t exist, causing a problem with
        // Windows build.
        // Ideally we should crash or log if target doesn't exist. More detail can be found at
        // D27587133.
        winFS.createSymbolicLink(symLink, target, isDirectory(target));
      } else {
        Files.createSymbolicLink(symLink, target);
      }
    } catch (IllegalArgumentException e) {
      // On windows, if one creates a symlink on a different filesystem, the error is not
      // particularly useful. ("'other' has different root"). Add some extra context
      if (e.getMessage().endsWith("has different root")) {
        String msg = String.format("Could not link %s to %s: %s", target, symLink, e.getMessage());
        throw new IllegalArgumentException(msg, e);
      } else {
        throw e;
      }
    }
  }

  public static void createSymLink(@Nullable WindowsFS winFS, AbsPath symLink, Path target)
      throws IOException {
    createSymLink(winFS, symLink.getPath(), target);
  }

  /**
   * Returns whether a path is a directory..
   *
   * @param path An absolute file name
   * @param linkOptions Link options
   * @return Whether the path is a directory.
   */
  public static boolean isDirectory(Path path, LinkOption... linkOptions) {
    return Files.isDirectory(normalize(path).toAbsolutePath(), linkOptions);
  }

  /**
   * Converts a path to an absolute Windows 'long path' as a string.
   * https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#maximum-path-length-limitation
   *
   * @param path The path to transform to a long path appropriate string form
   * @return The string representation of the path appropriate for long file name usage.
   */
  public static String getWindowsLongPathString(AbsPath path) {
    // The path must be normalized and absolutized first.
    return "\\\\?\\" + MorePaths.normalize(path);
  }
}
