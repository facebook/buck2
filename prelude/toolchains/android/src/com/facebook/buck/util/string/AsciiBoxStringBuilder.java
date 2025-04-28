/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.string;

import java.util.ArrayList;
import java.util.List;

/** Writes lines inside of an ASCII art box, as for important messages. */
public class AsciiBoxStringBuilder {
  private final List<String> lines = new ArrayList<>();
  private final int maxLength;

  public AsciiBoxStringBuilder(int maxLength) {
    this.maxLength = Math.max(1, maxLength);
  }

  public AsciiBoxStringBuilder writeLine(String line, Object... args) {
    if (args.length > 0) {
      line = String.format(line, args);
    }

    int lineBreakIndex = line.indexOf('\n');
    while (lineBreakIndex >= 0) {
      String subLine = line.substring(0, lineBreakIndex);
      writeLine(subLine);
      line = line.substring(lineBreakIndex + 1);
      lineBreakIndex = line.indexOf('\n');
    }

    while (line.length() > maxLength) {
      lineBreakIndex = line.indexOf(' ') + 1;
      if (lineBreakIndex == 0 || lineBreakIndex >= maxLength) {
        // No space? Just split it at max length
        lineBreakIndex = maxLength;
      }

      int nextSpaceIndex = lineBreakIndex;
      while (nextSpaceIndex < maxLength) {
        lineBreakIndex = nextSpaceIndex;

        // Set the split to be after the space, so that we don't end up with leading spaces
        nextSpaceIndex = line.indexOf(' ', lineBreakIndex) + 1;
        if (nextSpaceIndex == 0) {
          nextSpaceIndex = maxLength;
        }
      }

      writeLine(line.substring(0, lineBreakIndex));
      line = line.substring(lineBreakIndex);
    }

    line = line.replace('\t', ' ');
    lines.add(line);

    return this;
  }

  @Override
  public String toString() {
    return new Object() {
      private final StringBuilder stringBuilder = new StringBuilder();

      @Override
      public String toString() {
        writeEdge();
        writeLine("");

        for (String line : lines) {
          writeLine(line);
        }

        writeLine("");
        writeEdge();
        return stringBuilder.toString();
      }

      private void writeLine(String line) {
        stringBuilder.append("| ");
        stringBuilder.append(line);
        repeatCharacter(' ', maxLength - line.length());
        stringBuilder.append(" |\n");
      }

      private void writeEdge() {
        stringBuilder.append('+');
        repeatCharacter('-', maxLength + 2);
        stringBuilder.append("+\n");
      }

      private void repeatCharacter(char c, int times) {
        for (int i = 0; i < times; i++) {
          stringBuilder.append(c);
        }
      }
    }.toString();
  }
}
