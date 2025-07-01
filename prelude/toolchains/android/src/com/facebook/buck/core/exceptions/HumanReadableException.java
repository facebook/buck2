/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.core.exceptions;

import javax.annotation.Nullable;

/**
 * Exception with an error message that can sensibly be displayed to the user without a stacktrace.
 * This exception is meant only to be caught at the top level of the application.
 */
public class HumanReadableException extends RuntimeException
    implements ExceptionWithHumanReadableMessage {

  private final String humanReadableErrorMessage;
  private final DependencyStack dependencyStack;

  public HumanReadableException(
      DependencyStack dependencyStack, String humanReadableFormatString, Object... args) {
    this(dependencyStack, String.format(humanReadableFormatString, args));
  }

  public HumanReadableException(DependencyStack dependencyStack, String humanReadableErrorMessage) {
    this(null, dependencyStack, humanReadableErrorMessage);
  }

  public HumanReadableException(
      @Nullable Throwable cause,
      DependencyStack dependencyStack,
      String humanReadableErrorMessage) {
    super(humanReadableErrorMessage, cause);
    this.humanReadableErrorMessage = humanReadableErrorMessage;
    this.dependencyStack = dependencyStack;
  }

  public HumanReadableException(
      @Nullable Throwable cause,
      DependencyStack dependencyStack,
      String humanReadableFormatString,
      Object... args) {
    this(cause, dependencyStack, String.format(humanReadableFormatString, args));
  }

  public HumanReadableException(String humanReadableFormatString, Object... args) {
    this(String.format(humanReadableFormatString, args));
  }

  public HumanReadableException(String humanReadableErrorMessage) {
    this((Throwable) null /* cause */, humanReadableErrorMessage);
  }

  public HumanReadableException(@Nullable Throwable cause, String humanReadableErrorMessage) {
    super(humanReadableErrorMessage, cause);
    this.humanReadableErrorMessage = humanReadableErrorMessage;
    this.dependencyStack = DependencyStack.root();
  }

  public HumanReadableException(
      @Nullable Throwable cause, String humanReadableFormatString, Object... args) {
    this(cause, String.format(humanReadableFormatString, args));
  }

  public HumanReadableException(ExceptionWithHumanReadableMessage e) {
    this(
        (Throwable) ((e instanceof Throwable) ? e : null),
        e.getDependencyStack(),
        e.getHumanReadableErrorMessage());
  }

  @Override
  public String getHumanReadableErrorMessage() {
    return humanReadableErrorMessage;
  }

  @Override
  public DependencyStack getDependencyStack() {
    return dependencyStack;
  }
}
