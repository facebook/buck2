/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.io.file;

import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.core.util.immutables.BuckStyleValue;
import java.nio.file.Path;
import org.immutables.value.Value;

/** A contract for matching {@link Path}s. */
public interface PathMatcher {

  /** Check if given path matches pattern. */
  boolean matches(RelPath path);

  /**
   * Returns {@link PathOrGlob} object that contains a value that represents a path or glob pattern
   * identifying paths that should be matched by this matcher, and a way to figure out whether the
   * value is a Path or Glob.
   */
  PathOrGlob getPathOrGlob();

  /** Returns a glob pattern identifying paths that should be matched by this matcher. */
  String getGlob();

  /**
   * Wrapper around type of the path matcher's {@link #getPathOrGlob} return value (Path or Glob
   * type and a value).
   */
  @BuckStyleValue
  abstract class PathOrGlob {

    /** Type of the path matcher's {@link #getPathOrGlob} return value. Path or Glob */
    protected enum Type {
      PATH,
      GLOB
    }

    public abstract String getValue();

    protected abstract Type getType();

    @Value.Derived
    public boolean isPath() {
      return getType() == Type.PATH;
    }

    @Value.Derived
    public boolean isGlob() {
      return getType() == Type.GLOB;
    }

    public static PathOrGlob glob(String glob) {
      return ImmutablePathOrGlob.ofImpl(glob, Type.GLOB);
    }

    public static PathOrGlob path(RelPath path) {
      return ImmutablePathOrGlob.ofImpl(path.toString(), Type.PATH);
    }
  }
}
