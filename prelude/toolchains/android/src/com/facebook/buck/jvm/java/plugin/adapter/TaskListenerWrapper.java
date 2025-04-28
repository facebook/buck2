/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.plugin.adapter;

import com.sun.source.util.TaskEvent;
import com.sun.source.util.TaskListener;

public class TaskListenerWrapper implements TaskListener {
  private final TaskListener inner;

  public TaskListenerWrapper(TaskListener inner) {
    this.inner = inner;
  }

  @Override
  public void started(TaskEvent e) {
    if (inner != null) {
      inner.started(e);
    }
  }

  @Override
  public void finished(TaskEvent e) {
    if (inner != null) {
      inner.finished(e);
    }
  }
}
