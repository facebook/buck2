/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.io.file;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.junit.Assert.assertEquals;

import com.google.common.base.Strings;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.Optional;
import java.util.OptionalInt;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/** Unit tests for {@link PathListing}. */
public class PathListingTest {
  @Rule public TemporaryFolder tmpDir = new TemporaryFolder();

  private Path oldest;
  private Path middle;
  private Path newest;

  private void setupPaths(int middleFileSize) throws IOException {
    oldest = tmpDir.newFile("oldest").toPath();

    // Many filesystems only support second granularity for last-modified time.
    Files.setLastModifiedTime(oldest, FileTime.fromMillis(1000));

    middle = tmpDir.getRoot().toPath().resolve("middle");
    Files.write(middle, Strings.repeat("X", middleFileSize).getBytes(StandardCharsets.UTF_8));
    Files.setLastModifiedTime(middle, FileTime.fromMillis(2000));

    newest = tmpDir.newFile("newest").toPath();
    Files.setLastModifiedTime(newest, FileTime.fromMillis(3000));
  }

  @Test
  public void noPathsReturnsEmptyInclude() throws IOException {
    assertThat(
        PathListing.listMatchingPathsWithFilters(
            tmpDir.getRoot().toPath(),
            "*",
            PathListing.GET_PATH_MODIFIED_TIME,
            PathListing.FilterMode.INCLUDE,
            OptionalInt.empty(), // maxPathsFilter
            Optional.empty()), // maxSizeFilter
        empty());
  }

  @Test
  public void noPathsReturnsEmptyExclude() throws IOException {
    assertThat(
        PathListing.listMatchingPathsWithFilters(
            tmpDir.getRoot().toPath(),
            "*",
            PathListing.GET_PATH_MODIFIED_TIME,
            PathListing.FilterMode.EXCLUDE,
            OptionalInt.empty(), // maxPathsFilter
            Optional.empty()), // maxSizeFilter
        empty());
  }

  @Test
  public void listsEmptyIncludeZeroNumPaths() throws IOException {
    setupPaths(0);
    assertThat(
        PathListing.listMatchingPathsWithFilters(
            tmpDir.getRoot().toPath(),
            "*",
            PathListing.GET_PATH_MODIFIED_TIME,
            PathListing.FilterMode.INCLUDE,
            OptionalInt.of(0), // maxPathsFilter
            Optional.empty()), // maxSizeFilter
        empty());
  }

  @Test
  public void listsAllExcludeZeroNumPaths() throws IOException {
    setupPaths(0);
    assertEquals(
        PathListing.listMatchingPathsWithFilters(
            tmpDir.getRoot().toPath(),
            "*",
            PathListing.GET_PATH_MODIFIED_TIME,
            PathListing.FilterMode.EXCLUDE,
            OptionalInt.of(0), // maxPathsFilter
            Optional.empty()), // maxSizeFilter
        ImmutableSet.of(newest, middle, oldest));
  }

  @Test
  public void listsOneIncludeOneNumPaths() throws IOException {
    setupPaths(0);
    assertEquals(
        PathListing.listMatchingPathsWithFilters(
            tmpDir.getRoot().toPath(),
            "*",
            PathListing.GET_PATH_MODIFIED_TIME,
            PathListing.FilterMode.INCLUDE,
            OptionalInt.of(1), // maxPathsFilter
            Optional.empty()), // maxSizeFilter
        ImmutableSet.of(newest));
  }

  @Test
  public void listsTwoExcludeOneNumPaths() throws IOException {
    setupPaths(0);
    assertEquals(
        PathListing.listMatchingPathsWithFilters(
            tmpDir.getRoot().toPath(),
            "*",
            PathListing.GET_PATH_MODIFIED_TIME,
            PathListing.FilterMode.EXCLUDE,
            OptionalInt.of(1), // maxPathsFilter
            Optional.empty()), // maxSizeFilter
        ImmutableSet.of(middle, oldest));
  }

  @Test
  public void listsAllIncludeMaxIntPaths() throws IOException {
    setupPaths(0);
    assertEquals(
        PathListing.listMatchingPathsWithFilters(
            tmpDir.getRoot().toPath(),
            "*",
            PathListing.GET_PATH_MODIFIED_TIME,
            PathListing.FilterMode.INCLUDE,
            OptionalInt.of(Integer.MAX_VALUE), // maxPathsFilter
            Optional.empty()), // maxSizeFilter
        ImmutableSet.of(newest, middle, oldest));
  }

  @Test
  public void listsNoneExcludeMaxIntPaths() throws IOException {
    setupPaths(0);
    assertThat(
        PathListing.listMatchingPathsWithFilters(
            tmpDir.getRoot().toPath(),
            "*",
            PathListing.GET_PATH_MODIFIED_TIME,
            PathListing.FilterMode.EXCLUDE,
            OptionalInt.of(Integer.MAX_VALUE), // maxPathsFilter
            Optional.empty()), // maxSizeFilter
        empty());
  }

  @Test
  public void listsEmptyIncludeZeroSize() throws IOException {
    setupPaths(10);
    assertThat(
        PathListing.listMatchingPathsWithFilters(
            tmpDir.getRoot().toPath(),
            "*",
            PathListing.GET_PATH_MODIFIED_TIME,
            PathListing.FilterMode.INCLUDE,
            OptionalInt.empty(), // maxPathsFilter
            Optional.of(0L)), // maxSizeFilter
        empty());
  }

  @Test
  public void listsAllExcludeZeroSize() throws IOException {
    setupPaths(10);
    assertEquals(
        PathListing.listMatchingPathsWithFilters(
            tmpDir.getRoot().toPath(),
            "*",
            PathListing.GET_PATH_MODIFIED_TIME,
            PathListing.FilterMode.EXCLUDE,
            OptionalInt.empty(), // maxPathsFilter
            Optional.of(0L)), // maxSizeFilter
        ImmutableSet.of(newest, middle, oldest));
  }

  @Test
  public void listsOneIncludeOneSize() throws IOException {
    setupPaths(10);
    assertEquals(
        PathListing.listMatchingPathsWithFilters(
            tmpDir.getRoot().toPath(),
            "*",
            PathListing.GET_PATH_MODIFIED_TIME,
            PathListing.FilterMode.INCLUDE,
            OptionalInt.empty(), // maxPathsFilter
            Optional.of(10L)), // maxSizeFilter
        ImmutableSet.of(newest));
  }

  @Test
  public void listsTwoExcludeOneSize() throws IOException {
    setupPaths(10);
    assertEquals(
        PathListing.listMatchingPathsWithFilters(
            tmpDir.getRoot().toPath(),
            "*",
            PathListing.GET_PATH_MODIFIED_TIME,
            PathListing.FilterMode.EXCLUDE,
            OptionalInt.empty(), // maxPathsFilter
            Optional.of(10L)), // maxSizeFilter
        ImmutableSet.of(middle, oldest));
  }

  @Test
  public void listsAllIncludeMaxLongSize() throws IOException {
    setupPaths(10);
    assertEquals(
        PathListing.listMatchingPathsWithFilters(
            tmpDir.getRoot().toPath(),
            "*",
            PathListing.GET_PATH_MODIFIED_TIME,
            PathListing.FilterMode.INCLUDE,
            OptionalInt.empty(), // maxPathsFilter
            Optional.of(Long.MAX_VALUE)), // maxSizeFilter
        ImmutableSet.of(newest, middle, oldest));
  }

  @Test
  public void listsNoneExcludeMaxLongSize() throws IOException {
    setupPaths(10);
    assertThat(
        PathListing.listMatchingPathsWithFilters(
            tmpDir.getRoot().toPath(),
            "*",
            PathListing.GET_PATH_MODIFIED_TIME,
            PathListing.FilterMode.EXCLUDE,
            OptionalInt.empty(), // maxPathsFilter
            Optional.of(Long.MAX_VALUE)), // maxSizeFilter
        empty());
  }
}
