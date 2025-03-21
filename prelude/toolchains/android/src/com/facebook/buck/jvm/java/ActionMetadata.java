/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import java.nio.file.Path;
import java.util.Map;
import java.util.Objects;

public class ActionMetadata {

  private final Map<Path, String> previousDigest;
  private final Map<Path, String> currentDigest;

  public ActionMetadata(Map<Path, String> previousDigest, Map<Path, String> currentDigest) {
    this.previousDigest = previousDigest;
    this.currentDigest = currentDigest;
  }

  public Map<Path, String> getPreviousDigest() {
    return previousDigest;
  }

  public Map<Path, String> getCurrentDigest() {
    return currentDigest;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    ActionMetadata that = (ActionMetadata) o;
    return Objects.equals(previousDigest, that.previousDigest)
        && Objects.equals(currentDigest, that.currentDigest);
  }

  @Override
  public int hashCode() {
    return Objects.hash(previousDigest, currentDigest);
  }
}
