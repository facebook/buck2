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

import com.zaxxer.nuprocess.NuProcess;
import com.zaxxer.nuprocess.NuProcessHandler;
import java.nio.ByteBuffer;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

public class FakeNuProcess implements NuProcess {
  final int pid;
  int exitCode;
  final AtomicBoolean running = new AtomicBoolean(true);

  public FakeNuProcess(int pid) {
    this.pid = pid;
  }

  @Override
  public int getPID() {
    return pid;
  }

  @Override
  public boolean isRunning() {
    return running.get();
  }

  public void finish(int exitCode) {
    this.exitCode = exitCode;
    running.set(false);
  }

  @Override
  public int waitFor(long timeout, TimeUnit timeUnit) {
    running.set(false);
    return exitCode;
  }

  @Override
  public void wantWrite() {}

  @Override
  public void writeStdin(ByteBuffer buffer) {}

  @Override
  public void closeStdin(boolean force) {}

  @Override
  public boolean hasPendingWrites() {
    return false;
  }

  @Override
  public void destroy(boolean force) {}

  @Override
  public void setProcessHandler(NuProcessHandler processHandler) {}
}
