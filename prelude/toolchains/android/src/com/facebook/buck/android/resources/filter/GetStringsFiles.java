/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.resources.filter;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.facebook.buck.io.pathformat.PathFormatter;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.ImmutableList;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Path;
import java.util.function.Predicate;
import java.util.regex.Pattern;

/**
 * Generates a list of strings.xml files
 *
 * <p>The ordering of strings files is consistent with the order of the input resource directories
 */
public class GetStringsFiles {
  @VisibleForTesting
  public static final Pattern STRINGS_FILE_PATH =
      Pattern.compile("(\\b|.*/)res/values(-.+)*/strings.xml", Pattern.CASE_INSENSITIVE);

  public static ImmutableList<Path> getFiles(
      AbsPath root, DirectoryStream.Filter<? super Path> ignoreFilter, ImmutableList<Path> resDirs)
      throws IOException {
    Predicate<Path> isStringsFile =
        pathRelativeToProjectRoot -> {
          String filePath = PathFormatter.pathWithUnixSeparators(pathRelativeToProjectRoot);
          return STRINGS_FILE_PATH.matcher(filePath).matches();
        };

    ImmutableList.Builder<Path> stringFilesBuilder = ImmutableList.builder();
    for (Path resDir : resDirs) {
      stringFilesBuilder.addAll(
          ProjectFilesystemUtils.getFilesUnderPath(
              root,
              resDir,
              isStringsFile,
              ProjectFilesystemUtils.getDefaultVisitOptions(),
              ignoreFilter));
    }

    return stringFilesBuilder.build();
  }
}
