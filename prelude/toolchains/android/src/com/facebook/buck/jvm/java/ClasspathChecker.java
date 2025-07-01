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

import com.facebook.buck.io.file.PathListing;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Joiner;
import com.google.common.base.Splitter;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;
import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Locale;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

public class ClasspathChecker {

  private static final ImmutableSet<String> ALLOWED_EXTENSIONS_SET = ImmutableSet.of("jar", "zip");

  private final String separator;
  private final String pathSeparator;
  private final Function<String, Path> toPathFunc;
  private final Predicate<Path> isDirectoryFunc;
  private final Predicate<Path> isFileFunc;
  private final BiFunction<Path, String, Iterable<Path>> globberFunc;

  public ClasspathChecker() {
    this(
        File.separator,
        File.pathSeparator,
        Paths::get,
        Files::isDirectory,
        Files::isRegularFile,
        (path, glob) -> {
          try {
            return PathListing.listMatchingPaths(path, glob, PathListing.GET_PATH_MODIFIED_TIME);
          } catch (IOException e) {
            throw new UncheckedIOException(e);
          }
        });
  }

  @VisibleForTesting
  ClasspathChecker(
      String separator,
      String pathSeparator,
      Function<String, Path> toPathFunc,
      Predicate<Path> isDirectoryFunc,
      Predicate<Path> isFileFunc,
      BiFunction<Path, String, Iterable<Path>> globberFunc) {
    this.separator = separator;
    this.pathSeparator = pathSeparator;
    this.toPathFunc = toPathFunc;
    this.isDirectoryFunc = isDirectoryFunc;
    this.isFileFunc = isFileFunc;
    this.globberFunc = globberFunc;
  }

  /**
   * Parses a Java classpath string ("path/to/foo:baz.jar:blech.zip:path/to/*") and checks if at
   * least one entry is valid (exists on disk).
   *
   * <p>From http://docs.oracle.com/javase/8/docs/technotes/tools/windows/classpath.html :
   *
   * <p>Class path entries can contain the basename wildcard character *, which is considered
   * equivalent to specifying a list of all the files in the directory with the extension .jar or
   * .JAR. For example, the class path entry foo/* specifies all JAR files in the directory named
   * foo. A classpath entry consisting simply of * expands to a list of all the jar files in the
   * current directory.
   */
  public boolean validateClasspath(String classpath) {
    for (String entry : Splitter.on(pathSeparator).split(classpath)) {
      // On Windows, Path.endsWith("*") throws an error:
      //
      // java.nio.file.InvalidPathException: Illegal char <*> at index 0
      //
      // So, we split manually.
      List<String> classpathComponents = Splitter.on(separator).splitToList(entry);
      if (classpathComponents.isEmpty()) {
        continue;
      }

      if (Iterables.getLast(classpathComponents).equals("*")) {
        // Trim the * off the path.
        List<String> dirComponents = classpathComponents.subList(0, classpathComponents.size() - 1);
        Path entryDir = toPathFunc.apply(Joiner.on(separator).join(dirComponents));
        if (!Iterables.isEmpty(globberFunc.apply(entryDir, "*.jar"))) {
          return true;
        } else if (!Iterables.isEmpty(globberFunc.apply(entryDir, "*.JAR"))) {
          return true;
        }
      } else {
        Path entryPath = toPathFunc.apply(entry);
        if (isDirectoryFunc.test(entryPath)) {
          return true;
        } else if (isFileFunc.test(entryPath)
            && ALLOWED_EXTENSIONS_SET.contains(
                com.google.common.io.Files.getFileExtension(
                    entryPath.toString().toLowerCase(Locale.US)))) {
          return true;
        }
      }
    }
    return false;
  }
}
