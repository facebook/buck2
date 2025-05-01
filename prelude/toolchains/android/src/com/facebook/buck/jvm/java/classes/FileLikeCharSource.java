/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.classes;

import static java.nio.charset.StandardCharsets.UTF_8;

import com.google.common.io.CharSource;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

public class FileLikeCharSource extends CharSource {
  private final FileLike fileLike;

  public FileLikeCharSource(FileLike fileLike) {
    this.fileLike = fileLike;
  }

  @Override
  public Reader openStream() throws IOException {
    InputStream is = fileLike.getInput();
    return new InputStreamReader(is, UTF_8) {
      @Override
      public void close() throws IOException {
        super.close();
        is.close();
      }
    };
  }
}
