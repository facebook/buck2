/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.zip;

import java.io.IOException;
import java.io.OutputStream;
import java.util.zip.GZIPOutputStream;

public class BestCompressionGZIPOutputStream extends GZIPOutputStream {

  public BestCompressionGZIPOutputStream(OutputStream out, boolean syncFlush) throws IOException {
    super(out, syncFlush);
    def.setLevel(9);
  }
}
