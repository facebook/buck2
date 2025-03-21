/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util;

import com.facebook.buck.core.util.log.Logger;
import java.io.Closeable;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileAttribute;
import java.util.function.Supplier;

/** Creates a temporary file that is deleted when this object is closed. */
public class NamedTemporaryFile implements Closeable, Supplier<Path> {
  private static final Logger LOG = Logger.get(NamedTemporaryFile.class);

  private final Path tempPath;

  public NamedTemporaryFile(String prefix, String suffix, FileAttribute<?>... attrs)
      throws IOException {
    tempPath = Files.createTempFile(prefix, suffix, attrs);
  }

  @Override
  public Path get() {
    return tempPath;
  }

  @Override
  public synchronized void close() {
    if (Files.exists(tempPath)) {
      try {
        Files.delete(tempPath);
      } catch (IOException e) {
        LOG.warn(e, "Cannot delete time file: %s", tempPath);
      }
    }
  }
}
