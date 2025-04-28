/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util;

import com.facebook.buck.util.function.ThrowingSupplier;
import com.google.common.base.Preconditions;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

/** Fake implementation of {@link java.lang.Process}. */
public class FakeProcess extends Process {

  private final int exitValue;
  private final OutputStream outputStream;
  private final ByteArrayOutputStream outputMirror;
  private final InputStream inputStream;
  private final InputStream errorStream;
  private final Optional<ThrowingSupplier<Optional<InterruptedException>, RuntimeException>> waiter;
  private boolean isDestroyed;
  private boolean isWaitedFor;

  public FakeProcess(int exitValue) {
    this(exitValue, "", "");
  }

  public FakeProcess(int exitValue, String stdout, String stderr) {
    this(exitValue, stdout, stderr, Optional.empty());
  }

  public FakeProcess(
      Optional<ThrowingSupplier<Optional<InterruptedException>, RuntimeException>> waiter) {
    this(0, waiter);
  }

  public FakeProcess(
      int exitValue,
      Optional<ThrowingSupplier<Optional<InterruptedException>, RuntimeException>> waiter) {
    this(exitValue, "", "", waiter);
  }

  public FakeProcess(
      int exitValue, OutputStream outputStream, InputStream inputStream, InputStream errorStream) {
    this(exitValue, outputStream, inputStream, errorStream, Optional.empty());
  }

  public FakeProcess(
      int exitValue,
      String stdout,
      String stderr,
      Optional<ThrowingSupplier<Optional<InterruptedException>, RuntimeException>> waiter) {
    this(
        exitValue,
        new ByteArrayOutputStream(),
        new ByteArrayInputStream(
            Preconditions.checkNotNull(stdout).getBytes(StandardCharsets.UTF_8)),
        new ByteArrayInputStream(
            Preconditions.checkNotNull(stderr).getBytes(StandardCharsets.UTF_8)),
        waiter);
  }

  public FakeProcess(
      int exitValue,
      OutputStream outputStream,
      InputStream inputStream,
      InputStream errorStream,
      Optional<ThrowingSupplier<Optional<InterruptedException>, RuntimeException>> waitSupplier) {
    this.exitValue = exitValue;
    this.outputStream = Preconditions.checkNotNull(outputStream);
    this.outputMirror = new ByteArrayOutputStream();
    this.inputStream = Preconditions.checkNotNull(inputStream);
    this.errorStream = Preconditions.checkNotNull(errorStream);
    this.waiter = waitSupplier;
  }

  @Override
  public void destroy() {
    isDestroyed = true;
  }

  @Override
  public int exitValue() {
    if (!isWaitedFor) {
      throw new IllegalThreadStateException();
    }
    return exitValue;
  }

  @Override
  public OutputStream getOutputStream() {
    return new TeeOutputStream(outputStream, outputMirror);
  }

  @Override
  public InputStream getInputStream() {
    return inputStream;
  }

  @Override
  public InputStream getErrorStream() {
    return errorStream;
  }

  @Override
  public int waitFor() throws InterruptedException {
    if (!isWaitedFor) {
      isWaitedFor = true;
      if (waiter.isPresent()) {
        ThrowingSupplier<Optional<InterruptedException>, RuntimeException> supplier = waiter.get();
        Optional<InterruptedException> interruptedExceptionOptional = supplier.get();
        if (interruptedExceptionOptional.isPresent()) {
          throw interruptedExceptionOptional.get();
        }
      }
    }
    return exitValue;
  }

  @Override
  public boolean waitFor(long timeout, TimeUnit unit) throws InterruptedException {
    waitFor();
    return true;
  }

  /** Returns true if {@link #destroy()} was called on this object, false otherwise. */
  public boolean isDestroyed() {
    return isDestroyed;
  }

  /** Returns what has been written to {@link #getOutputStream()} so far. */
  public String getOutput() {
    return outputMirror.toString();
  }
}
