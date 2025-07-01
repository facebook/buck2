/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util;

import java.io.IOException;
import java.io.OutputStream;

/** An OutputStream which forwards to two OutputStreams. */
public class TeeOutputStream extends OutputStream {
  private final OutputStream one;
  private final OutputStream two;

  public TeeOutputStream(OutputStream one, OutputStream two) {
    this.one = one;
    this.two = two;
  }

  @Override
  public void write(int b) throws IOException {
    one.write(b);
    two.write(b);
  }

  @Override
  public void flush() throws IOException {
    one.flush();
    two.flush();
  }

  @Override
  public void close() throws IOException {
    one.close();
    two.close();
  }
}
