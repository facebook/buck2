/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.io.file;

import com.google.common.base.Functions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;
import com.google.common.collect.Ordering;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.Optional;
import java.util.OptionalInt;

/** Utility class to list files which match a pattern, applying ordering and filtering. */
public class PathListing {
  // Utility class, do not instantiate.
  private PathListing() {}

  /** Whether to include files which match the filter, or exclude them. */
  public enum FilterMode {
    INCLUDE,
    EXCLUDE,
  }

  /** Fetches last-modified time from a path. */
  public interface PathModifiedTimeFetcher {
    FileTime getLastModifiedTime(Path path) throws IOException;
  }

  /** Uses {@code Files.getLastModifiedTime()} to get the last modified time for a Path. */
  public static final PathModifiedTimeFetcher GET_PATH_MODIFIED_TIME =
      path -> Files.getLastModifiedTime(path);

  /** Lists matching paths in descending modified time order. */
  public static ImmutableSortedSet<Path> listMatchingPaths(
      Path pathToGlob, String globPattern, PathModifiedTimeFetcher pathModifiedTimeFetcher)
      throws IOException {
    return listMatchingPathsWithFilters(
        pathToGlob,
        globPattern,
        pathModifiedTimeFetcher,
        FilterMode.INCLUDE,
        OptionalInt.empty(),
        Optional.empty());
  }

  /**
   * Lists paths in descending modified time order, excluding any paths which bring the number of
   * files over {@code maxNumPaths} or over {@code totalSizeFilter} bytes in size.
   */
  public static ImmutableSortedSet<Path> listMatchingPathsWithFilters(
      Path pathToGlob,
      String globPattern,
      PathModifiedTimeFetcher pathModifiedTimeFetcher,
      FilterMode filterMode,
      OptionalInt maxPathsFilter,
      Optional<Long> totalSizeFilter)
      throws IOException {

    // Fetch the modification time of each path and build a map of
    // (path => modification time) pairs.
    ImmutableMap.Builder<Path, FileTime> pathFileTimesBuilder = ImmutableMap.builder();
    try (DirectoryStream<Path> stream = Files.newDirectoryStream(pathToGlob, globPattern)) {
      for (Path path : stream) {
        try {
          pathFileTimesBuilder.put(path, pathModifiedTimeFetcher.getLastModifiedTime(path));
        } catch (NoSuchFileException e) {
          // Ignore the path.
          continue;
        }
      }
    }
    ImmutableMap<Path, FileTime> pathFileTimes = pathFileTimesBuilder.build();

    ImmutableSortedSet<Path> paths =
        ImmutableSortedSet.copyOf(
            Ordering.natural()
                // Order the keys of the map (the paths) by their values (the file modification
                // times).
                .onResultOf(Functions.forMap(pathFileTimes))
                // If two keys of the map have the same value, fall back to key order.
                .compound(Ordering.natural())
                // Use descending order.
                .reverse(),
            pathFileTimes.keySet());

    paths = applyNumPathsFilter(paths, filterMode, maxPathsFilter);
    paths = applyTotalSizeFilter(paths, filterMode, totalSizeFilter);
    return paths;
  }

  private static ImmutableSortedSet<Path> applyNumPathsFilter(
      ImmutableSortedSet<Path> paths, FilterMode filterMode, OptionalInt maxPathsFilter) {
    if (maxPathsFilter.isPresent()) {
      int limitIndex = Math.min(maxPathsFilter.getAsInt(), paths.size());
      paths = subSet(paths, filterMode, limitIndex);
    }
    return paths;
  }

  @SuppressWarnings("PMD.EmptyCatchBlock")
  private static ImmutableSortedSet<Path> applyTotalSizeFilter(
      ImmutableSortedSet<Path> paths, FilterMode filterMode, Optional<Long> totalSizeFilter)
      throws IOException {
    if (totalSizeFilter.isPresent()) {
      int limitIndex = 0;
      long totalSize = 0;
      for (Path path : paths) {
        try {
          totalSize += Files.size(path);
        } catch (NoSuchFileException e) {
          // Path was deleted; ignore it.
        }

        if (totalSize < totalSizeFilter.get()) {
          limitIndex++;
        } else {
          break;
        }
      }
      paths = subSet(paths, filterMode, limitIndex);
    }
    return paths;
  }

  private static ImmutableSortedSet<Path> subSet(
      ImmutableSortedSet<Path> paths, FilterMode filterMode, int limitIndex) {
    // This doesn't copy the contents of the ImmutableSortedSet. We use it
    // as a simple way to get O(1) access to the set's contents, as otherwise
    // we would have to iterate to find the Nth element.
    ImmutableList<Path> pathsList = paths.asList();
    boolean fullSet = limitIndex == paths.size();
    switch (filterMode) {
      case INCLUDE:
        // Make sure we don't call pathsList.get(pathsList.size()).
        if (!fullSet) {
          paths = paths.headSet(pathsList.get(limitIndex));
        }
        break;
      case EXCLUDE:
        if (fullSet) {
          // Make sure we don't call pathsList.get(pathsList.size()).
          paths = ImmutableSortedSet.of();
        } else {
          paths = paths.tailSet(pathsList.get(limitIndex));
        }
        break;
    }
    return paths;
  }
}
