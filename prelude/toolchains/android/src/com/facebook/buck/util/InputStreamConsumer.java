/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util;

import com.facebook.buck.util.types.Unit;
import com.google.common.base.StandardSystemProperty;
import com.google.common.collect.ImmutableList;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.Reader;
import java.util.concurrent.Callable;

/** An utility to process input stream with a list of line-by-line consumers */
public final class InputStreamConsumer implements Callable<Unit> {

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
  public Unit call() throws IOException {
    String line;
    while ((line = inputReader.readLine()) != null) {
      for (Handler handler : handlers) {
        handler.handleLine(line);
      }
    }
    return Unit.UNIT;
  }

  public static Handler createAnsiHighlightingHandler(
      boolean flagOutputWrittenToStream, PrintStream printStream, Ansi ansi) {
    return new HighlightingOutput(flagOutputWrittenToStream, printStream, ansi);
  }

  private static class HighlightingOutput implements Handler {

    private static final String LINE_SEPARATOR = StandardSystemProperty.LINE_SEPARATOR.value();
    private final boolean flagOutputWrittenToStream;
    private final PrintStream printStream;
    private final Ansi ansi;

    public HighlightingOutput(
        boolean flagOutputWrittenToStream, PrintStream printStream, Ansi ansi) {
      this.flagOutputWrittenToStream = flagOutputWrittenToStream;
      this.printStream = printStream;
      this.ansi = ansi;
    }

    @Override
    public void handleLine(String line) {
      String highlightOn = flagOutputWrittenToStream ? ansi.getHighlightedWarningSequence() : "";
      String highlightOff = flagOutputWrittenToStream ? ansi.getHighlightedResetSequence() : "";

      // We pass `line + LINE_SEPARATOR` to print() rather than invoke println() because
      // we want the line and the separator to be guaranteed to be printed together.
      // println() is implemented by calling print() then newLine(). Because those calls could be
      // interleaved when stdout and stderr are being consumed simultaneously (and I have seen
      // this happen), then you could end up with confusing output when stdout and stderr are
      // connected to the same terminal (which is often the case).
      printStream.print(highlightOn + line + LINE_SEPARATOR + highlightOff);
    }
  }
}
