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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.FilterOutputStream;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import org.junit.Test;

public class TeeOutputStreamTest {
  @Test
  public void delegates() throws Exception {
    CloseRecordingByteArrayOutputStream one = new CloseRecordingByteArrayOutputStream();
    CloseRecordingByteArrayOutputStream two = new CloseRecordingByteArrayOutputStream();
    TeeOutputStream teeOutputStream = new TeeOutputStream(one, two);
    teeOutputStream.write("foo".getBytes(StandardCharsets.UTF_8));
    teeOutputStream.flush();
    assertEquals("foo", one.toUtf8String());
    assertEquals("foo", two.toUtf8String());

    teeOutputStream.close();
    assertTrue(one.isClosed());
    assertTrue(two.isClosed());
  }

  static class CloseRecordingByteArrayOutputStream extends FilterOutputStream {
    private final ByteArrayOutputStream delegate;

    public CloseRecordingByteArrayOutputStream() {
      this(new ByteArrayOutputStream());
    }

    private CloseRecordingByteArrayOutputStream(ByteArrayOutputStream delegate) {
      super(delegate);
      this.delegate = delegate;
    }

    private boolean isClosed = false;

    @Override
    public void close() {
      isClosed = true;
    }

    public boolean isClosed() {
      return isClosed;
    }

    public String toUtf8String() {
      try {
        return delegate.toString(StandardCharsets.UTF_8.toString());
      } catch (UnsupportedEncodingException e) {
        throw new IllegalStateException(e);
      }
    }
  }
}
