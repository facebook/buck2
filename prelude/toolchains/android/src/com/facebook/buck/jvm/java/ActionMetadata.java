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

import java.nio.file.Path;
import java.util.Map;
import java.util.Objects;
import javax.annotation.Nullable;

public class ActionMetadata {

  private final Map<Path, String> previousDigest;
  private final Map<Path, String> currentDigest;
  private final Path incrementalMetadataFilePath;

  public ActionMetadata(
      Path incrementalMetadataFilePath,
      Map<Path, String> previousDigest,
      Map<Path, String> currentDigest) {
    this.previousDigest = previousDigest;
    this.currentDigest = currentDigest;
    this.incrementalMetadataFilePath = incrementalMetadataFilePath;
  }

  public Map<Path, String> getPreviousDigest() {
    return previousDigest;
  }

  public Map<Path, String> getCurrentDigest() {
    return currentDigest;
  }

  @Nullable
  public String getPreviousIncrementalMetadataDigest() {
    return previousDigest.get(incrementalMetadataFilePath);
  }

  @Nullable
  public String getCurrentIncrementalMetadataDigest() {
    return currentDigest.get(incrementalMetadataFilePath);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    ActionMetadata that = (ActionMetadata) o;
    return Objects.equals(previousDigest, that.previousDigest)
        && Objects.equals(currentDigest, that.currentDigest)
        && Objects.equals(incrementalMetadataFilePath, that.incrementalMetadataFilePath);
  }

  @Override
  public int hashCode() {
    return Objects.hash(previousDigest, currentDigest, incrementalMetadataFilePath);
  }
}
