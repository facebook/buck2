/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.core.exceptions;

import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;

import org.junit.Test;

public class HumanReadableExceptionsTest {

  @Test
  public void rethrowIfHumanReadableUnchecked_HumanReadableException() {
    Throwable e = new HumanReadableException("oh my");
    try {
      HumanReadableExceptions.throwIfHumanReadableUnchecked(e);
      fail("expecting to be rethrown");
    } catch (HumanReadableException x) {
      assertSame(e, x);
    }
  }

  @Test
  public void rethrowIfHumanReadableUnchecked_HumanReadableException_subclass() {
    class SubclassException extends HumanReadableException {

      public SubclassException() {
        super("test test");
      }
    }

    Throwable e = new SubclassException();
    try {
      HumanReadableExceptions.throwIfHumanReadableUnchecked(e);
      fail("expecting to be rethrown");
    } catch (SubclassException x) {
      assertSame(e, x);
    }
  }

  @Test
  public void rethrowIfHumanReadableUnchecked_ExceptionWithHumanReadableMessage_unchecked() {
    class UncheckedException extends RuntimeException implements ExceptionWithHumanReadableMessage {

      @Override
      public String getHumanReadableErrorMessage() {
        return "message to human";
      }
    }

    Throwable e = new UncheckedException();
    try {
      HumanReadableExceptions.throwIfHumanReadableUnchecked(e);
      fail("expecting to be rethrown");
    } catch (UncheckedException x) {
      assertSame(e, x);
    }
  }

  @Test
  public void rethrowIfHumanReadableUnchecked_othersAreNotRethrown() {
    HumanReadableExceptions.throwIfHumanReadableUnchecked(new RuntimeException());
    HumanReadableExceptions.throwIfHumanReadableUnchecked(new ClassCastException());

    class CheckedException extends Exception implements ExceptionWithHumanReadableMessage {
      @Override
      public String getHumanReadableErrorMessage() {
        return "another message";
      }
    }

    HumanReadableExceptions.throwIfHumanReadableUnchecked(new CheckedException());
  }
}
