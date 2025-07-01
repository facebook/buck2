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

import java.util.Optional;
import javax.annotation.Nullable;

/**
 * Allows wrapping a checked exception into an unchecked exception in a way that Buck understands.
 *
 * <p>Users should prefer {@code throw new BuckUncheckedExecutionException(exception);} to {@code
 * throw new RuntimeException(exception);} for throwing arbitrary checked exceptions as unchecked.
 * Even better would be to specify that the function throws BuckExecutionException.
 */
public class BuckUncheckedExecutionException extends RuntimeException
    implements ExceptionWithContext, WrapsException {
  private @Nullable final String context;

  public BuckUncheckedExecutionException(String message) {
    this(message, null);
  }

  public BuckUncheckedExecutionException(String message, @Nullable String context) {
    super(message);
    this.context = context;
  }

  public BuckUncheckedExecutionException(
      String message, @Nullable Throwable cause, @Nullable String context) {
    super(message, cause);
    this.context = context;
  }

  public BuckUncheckedExecutionException(Throwable cause) {
    this(cause, null);
  }

  public BuckUncheckedExecutionException(Throwable cause, @Nullable String context) {
    super(cause);
    this.context = context;
  }

  @Override
  public String getMessage() {
    StringBuilder builder = new StringBuilder();
    builder.append(context);
    String parentMessage = super.getMessage();
    if (parentMessage != null && !parentMessage.isEmpty()) {
      builder.append(": ");
      builder.append(parentMessage);
    }
    return builder.toString();
  }

  public BuckUncheckedExecutionException(Throwable cause, String format, Object... args) {
    this(cause, String.format(format, args));
  }

  @Override
  public Optional<String> getContext() {
    return Optional.ofNullable(context);
  }
}
