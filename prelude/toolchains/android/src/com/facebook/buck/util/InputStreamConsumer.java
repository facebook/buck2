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

import com.google.common.base.StandardSystemProperty;
import com.google.common.collect.ImmutableList;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.Reader;
import java.util.concurrent.Callable;

/** An utility to process input stream with a list of line-by-line consumers */
public final class InputStreamConsumer implements Callable<Void> {

  /** Interface to handle a line of input from the stream. */
  public interface Handler {
    void handleLine(String line);
  }

  private final LineFetcher inputReader;
  private final ImmutableList<Handler> handlers;

  public InputStreamConsumer(InputStream inputStream, Handler... handlers) {
    this(new InputStreamReader(inputStream), handlers);
  }

  public InputStreamConsumer(Reader reader, Handler... handlers) {
    this.inputReader = new LineFetcher(reader);
    this.handlers = ImmutableList.copyOf(handlers);
  }

  @Override
  public Void call() throws IOException {
    String line;
    while ((line = inputReader.readLine()) != null) {
      for (Handler handler : handlers) {
        handler.handleLine(line);
      }
    }
    return null;
  }

  public static Handler createAnsiHighlightingHandler(PrintStream printStream) {
    return new HighlightingOutput(printStream);
  }

  private static class HighlightingOutput implements Handler {

    private static final String LINE_SEPARATOR = StandardSystemProperty.LINE_SEPARATOR.value();
    private final PrintStream printStream;

    public HighlightingOutput(PrintStream printStream) {
      this.printStream = printStream;
    }

    @Override
    public void handleLine(String line) {
      // We pass `line + LINE_SEPARATOR` to print() rather than invoke println() because
      // we want the line and the separator to be guaranteed to be printed together.
      // println() is implemented by calling print() then newLine(). Because those calls could be
      // interleaved when stdout and stderr are being consumed simultaneously (and I have seen
      // this happen), then you could end up with confusing output when stdout and stderr are
      // connected to the same terminal (which is often the case).
      printStream.print(line + LINE_SEPARATOR);
    }
  }
}
