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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;
import javax.annotation.Nullable;

public class LineFetcher implements AutoCloseable {

  static final int BUFFER_LENGTH = 1024;
  private char[] readBuffer = new char[BUFFER_LENGTH];
  private char[] dosLineEndingCheckBuffer = new char[1];

  private final PushbackReader inputReader;

  LineFetcher(Reader baseReader) {
    inputReader =
        new PushbackReader(
            new BufferedReader(baseReader), BUFFER_LENGTH + dosLineEndingCheckBuffer.length);
  }

  @Nullable
  String readLine() throws IOException {
    int read = inputReader.read(readBuffer);
    if (read == -1) {
      return null;
    }

    StringBuilder builder = new StringBuilder();
    while (read != -1) {
      for (int lineEnd = 0; lineEnd < read; lineEnd++) {
        if (isLineEnd(readBuffer[lineEnd])) {
          pushUnreadBack(lineEnd, read);
          builder.append(new String(readBuffer, 0, lineEnd));
          return builder.toString();
        }
      }
      builder.append(new String(readBuffer, 0, read));
      read = inputReader.read(readBuffer);
    }

    return builder.toString();
  }

  private boolean isLineEnd(char c) {
    return c == '\r' || c == '\n';
  }

  private void pushUnreadBack(int lineEnd, int read) throws IOException {
    int startOfPushback = lineEnd + 1;

    if (readBuffer[lineEnd] == '\r') {
      if (lineEnd == read - 1) {
        handlePossibleWindowsLineEndingWithUnreadNewline();
      } else if (readBuffer[startOfPushback] == '\n') {
        startOfPushback++;
      }
    }

    inputReader.unread(readBuffer, startOfPushback, read - startOfPushback);
  }

  private void handlePossibleWindowsLineEndingWithUnreadNewline() throws IOException {
    inputReader.read(dosLineEndingCheckBuffer, 0, 1);
    if (dosLineEndingCheckBuffer[0] == '\n') {
      return;
    }

    inputReader.unread(dosLineEndingCheckBuffer);
  }

  @Override
  public void close() throws Exception {
    if (inputReader != null) {
      inputReader.close();
    }
  }
}
