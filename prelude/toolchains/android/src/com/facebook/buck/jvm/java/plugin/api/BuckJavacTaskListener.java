/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.plugin.api;

/**
 * Listens to events coming from the Java compiler. This is analogous to {@link
 * com.sun.source.util.TaskListener}, but loads in Buck's {@link ClassLoader} instead of the
 * compiler's.
 */
public interface BuckJavacTaskListener {
  static BuckJavacTaskListener wrapRealTaskListener(PluginClassLoader loader, Object taskListener) {
    Class<? extends BuckJavacTaskListener> taskListenerProxyClass =
        loader.loadClass(
            "com.facebook.buck.jvm.java.plugin.adapter.TaskListenerProxy",
            BuckJavacTaskListener.class);

    try {
      return taskListenerProxyClass.getConstructor(Object.class).newInstance(taskListener);
    } catch (ReflectiveOperationException e) {
      throw new RuntimeException(e);
    }
  }

  void started(TaskEventMirror e);

  void finished(TaskEventMirror e);
}
