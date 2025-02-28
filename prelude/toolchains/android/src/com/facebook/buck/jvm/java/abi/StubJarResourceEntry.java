/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi;

import java.nio.file.Path;
import java.util.Collections;
import java.util.List;

class StubJarResourceEntry extends StubJarEntry {
  private final LibraryReader input;
  private final Path path;

  public static StubJarResourceEntry of(LibraryReader input, Path path) {
    return new StubJarResourceEntry(input, path);
  }

  private StubJarResourceEntry(LibraryReader input, Path path) {
    this.input = input;
    this.path = path;
  }

  @Override
  public void write(StubJarWriter writer) {
    writer.writeEntry(path, () -> input.openResourceFile(path));
  }

  @Override
  public List<String> getInlineFunctions() {
    return Collections.emptyList();
  }

  @Override
  public boolean extendsInlineFunctionScope() {
    return false;
  }
}
