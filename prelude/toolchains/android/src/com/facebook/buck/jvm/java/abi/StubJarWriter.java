/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi;

import com.facebook.buck.util.function.ThrowingSupplier;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;

/** An interface for writing to stub jars. */
interface StubJarWriter extends AutoCloseable {
  void writeEntry(Path relativePath, ThrowingSupplier<InputStream, IOException> streamSupplier);

  @Override
  void close() throws IOException;
}
