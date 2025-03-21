/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd.serialization;

import java.nio.file.Path;
import java.nio.file.Paths;

/** {@link Path} to protobuf serializer */
public class PathSerializer {

  private PathSerializer() {}

  /** Serializes {@link Path} into javacd model */
  public static String serialize(Path path) {
    return path.toString();
  }

  /** Deserializes javacd model into {@link Path}. */
  public static Path deserialize(String path) {
    return Paths.get(path);
  }
}
