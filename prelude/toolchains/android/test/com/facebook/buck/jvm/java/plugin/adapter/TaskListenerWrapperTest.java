/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.plugin.adapter;

import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiTestRunner;
import com.sun.source.util.TaskEvent;
import com.sun.source.util.TaskListener;
import org.easymock.EasyMock;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(CompilerTreeApiTestRunner.class)
public class TaskListenerWrapperTest {
  private final TaskListener innerListener = EasyMock.createMock(TaskListener.class);

  @Test
  public void testChainsToInnerOnStart() {
    TaskListenerWrapper wrapper = new TaskListenerWrapper(innerListener);
    TaskEvent e = new TaskEvent(TaskEvent.Kind.PARSE);

    innerListener.started(e);

    EasyMock.replay(innerListener);

    wrapper.started(e);

    EasyMock.verify(innerListener);
  }

  @Test
  public void testChainsToInnerOnFinish() {
    TaskListenerWrapper wrapper = new TaskListenerWrapper(innerListener);
    TaskEvent e = new TaskEvent(TaskEvent.Kind.PARSE);

    innerListener.finished(e);

    EasyMock.replay(innerListener);

    wrapper.finished(e);

    EasyMock.verify(innerListener);
  }

  @Test
  public void testIgnoresNullListeners() {
    TaskListenerWrapper wrapper = new TaskListenerWrapper(null);
    TaskEvent e = new TaskEvent(TaskEvent.Kind.PARSE);

    wrapper.started(e);
    wrapper.finished(e);

    // Expect no crashes
  }
}
