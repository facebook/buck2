/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.util.xml;

import java.util.Objects;

class DocumentLocation {

  private final Integer lineNumber;
  private final Integer columnNumber;

  DocumentLocation(final Integer lineNumber, final Integer columnNumber) {
    this.lineNumber = lineNumber;
    this.columnNumber = columnNumber;
  }

  public Integer getLineNumber() {
    return lineNumber;
  }

  public Integer getColumnNumber() {
    return columnNumber;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    DocumentLocation that = (DocumentLocation) o;
    return Objects.equals(lineNumber, that.lineNumber)
        && Objects.equals(columnNumber, that.columnNumber);
  }

  @Override
  public int hashCode() {
    return Objects.hash(lineNumber, columnNumber);
  }

  static DocumentLocation of(Integer lineNumber, Integer columnNumber) {
    return new DocumentLocation(lineNumber, columnNumber);
  }
}
