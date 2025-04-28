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
import java.util.concurrent.CopyOnWriteArraySet;

/**
 * Multiplexes messages to several other task listeners. Messages are dispatched to listeners in the
 * order in which the listeners were registered.
 */
class MultiplexingTaskListener implements TaskListener {
  private final CopyOnWriteArraySet<TaskListener> listeners = new CopyOnWriteArraySet<>();

  public void addListener(TaskListener listener) {
    listeners.add(listener);
  }

  public void removeListener(TaskListener listener) {
    listeners.remove(listener);
  }

  @Override
  public void started(TaskEvent e) {
    for (TaskListener listener : listeners) {
      listener.started(e);
    }
  }

  @Override
  public void finished(TaskEvent e) {
    for (TaskListener listener : listeners) {
      listener.finished(e);
    }
  }
}
