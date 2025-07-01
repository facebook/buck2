/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.abi;

import java.nio.file.Path;
import java.util.Collections;
import java.util.List;

/** Write an entry to stub jar by directly copy an existing abi. */
class StubJarExistingEntry extends StubJarEntry {
  private final LibraryReader existingAbiInput;
  private final Path path;

  public static StubJarExistingEntry of(LibraryReader existingAbiInput, Path path) {
    return new StubJarExistingEntry(existingAbiInput, path);
  }

  private StubJarExistingEntry(LibraryReader existingAbiInput, Path path) {
    this.existingAbiInput = existingAbiInput;
    this.path = path;
  }

  @Override
  public void write(StubJarWriter writer) {
    writer.writeEntry(path, () -> existingAbiInput.openInputStream(path));
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
