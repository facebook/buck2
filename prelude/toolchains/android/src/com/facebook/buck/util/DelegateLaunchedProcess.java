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

import com.google.common.collect.ImmutableList;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * {@link ProcessExecutor.LaunchedProcess} interface implementation based on delegate {@link
 * ProcessExecutor.LaunchedProcess} instance.
 */
public abstract class DelegateLaunchedProcess implements ProcessExecutor.LaunchedProcess {

  private final ProcessExecutor.LaunchedProcess delegate;

  public DelegateLaunchedProcess(ProcessExecutor.LaunchedProcess delegate) {
    this.delegate = delegate;
  }

  @Override
  public boolean isAlive() {
    return delegate.isAlive();
  }

  @Override
  public ImmutableList<String> getCommand() {
    return delegate.getCommand();
  }

  @Override
  public OutputStream getStdin() {
    return delegate.getStdin();
  }

  @Override
  public InputStream getStdout() {
    return delegate.getStdout();
  }

  @Override
  public InputStream getStderr() {
    return delegate.getStderr();
  }

  @Override
  public void close() {
    delegate.close();
  }

  public ProcessExecutor.LaunchedProcess getDelegate() {
    return delegate;
  }
}
