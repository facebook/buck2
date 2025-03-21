/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.zip.collect;

import com.google.common.collect.ImmutableList;
import java.util.Objects;

/**
 * A set of sources to store as entries in a zip file. A source entry can be either a file or an
 * entry from another zip file.
 */
public class ZipEntrySourceCollection {

  private final ImmutableList<ZipEntrySource> sources;

  public ZipEntrySourceCollection(ImmutableList<ZipEntrySource> sources) {
    this.sources = sources;
  }

  public ImmutableList<ZipEntrySource> getSources() {
    return sources;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ZipEntrySourceCollection that = (ZipEntrySourceCollection) o;
    return Objects.equals(sources, that.sources);
  }

  @Override
  public int hashCode() {
    return Objects.hash(sources);
  }
}
