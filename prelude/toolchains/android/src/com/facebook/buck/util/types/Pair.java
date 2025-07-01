/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util.types;

import java.lang.ref.WeakReference;
import java.util.Objects;
import javax.annotation.Nullable;

/**
 * Simple type representing a pair of differently typed values.
 *
 * <p>Used to represent pair-like structures in coerced values.
 */
public class Pair<FIRST, SECOND> {

  private FIRST first;
  private SECOND second;

  private volatile boolean hashCodeComputed = false;
  private int hashCode;
  @Nullable private WeakReference<String> stringCache = null;

  public Pair(FIRST first, SECOND second) {
    this.first = first;
    this.second = second;
  }

  public FIRST getFirst() {
    return first;
  }

  public SECOND getSecond() {
    return second;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pair)) {
      return false;
    }

    Pair<?, ?> that = (Pair<?, ?>) other;
    return Objects.equals(this.first, that.first) && Objects.equals(this.second, that.second);
  }

  @Override
  public int hashCode() {
    if (!hashCodeComputed) {
      synchronized (this) {
        if (!hashCodeComputed) {
          hashCode = Objects.hash(first, second);
          hashCodeComputed = true;
        }
      }
    }

    return hashCode;
  }

  @Override
  public String toString() {
    synchronized (this) {
      if (stringCache == null) {
        return createStringAndCache();
      }
      String string = stringCache.get();
      if (string == null) {
        return createStringAndCache();
      }
      return string;
    }
  }

  private String createStringAndCache() {
    String string = String.format("Pair(%s, %s)", first, second);
    stringCache = new WeakReference<>(string);
    return string;
  }
}
