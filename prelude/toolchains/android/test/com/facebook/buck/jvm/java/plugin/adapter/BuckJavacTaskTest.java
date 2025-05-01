/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.plugin.adapter;

import static org.hamcrest.MatcherAssert.assertThat;

import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiTestRunner;
import com.facebook.buck.jvm.java.testutil.compiler.TestCompiler;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(CompilerTreeApiTestRunner.class)
public class BuckJavacTaskTest {
  @Rule public TestCompiler testCompiler = new TestCompiler();

  @Before
  public void setUp() throws Exception {
    testCompiler.addSourceFileContents("Foo.java", "class Foo { }");
  }

  @Test
  public void testTaskListenersGetEventsInOrder() throws IOException {
    BuckJavacTask javacTask = testCompiler.getJavacTask();
    List<String> events = new ArrayList<>();

    RecordingTaskListener tl1 = new RecordingTaskListener("1", events);
    RecordingTaskListener tl2 = new RecordingTaskListener("2", events);
    RecordingTaskListener tl3 = new RecordingTaskListener("3", events);

    javacTask.setProcessors(Collections.emptyList());
    TestTaskListenerAdapter.addTaskListener(javacTask, tl1);
    TestTaskListenerAdapter.addTaskListener(javacTask, tl2);
    TestTaskListenerAdapter.addTaskListener(javacTask, tl3);
    javacTask.enter();

    assertThat(
        events,
        Matchers.contains(
            "1: started PARSE",
            "2: started PARSE",
            "3: started PARSE",
            "1: finished PARSE",
            "2: finished PARSE",
            "3: finished PARSE",
            "1: started ENTER",
            "2: started ENTER",
            "3: started ENTER",
            "1: finished ENTER",
            "2: finished ENTER",
            "3: finished ENTER"));
  }

  @Test
  public void testTaskListenersCanUnregister() throws IOException {
    BuckJavacTask javacTask = testCompiler.getJavacTask();
    List<String> events = new ArrayList<>();

    RecordingTaskListener tl1 = new RecordingTaskListener("1", events);
    RecordingTaskListener tl2 =
        new RecordingTaskListener("2", events) {
          @Override
          public void finished(String event) {
            super.finished(event);
            TestTaskListenerAdapter.removeTaskListener(javacTask, this);
          }
        };
    RecordingTaskListener tl3 = new RecordingTaskListener("3", events);

    javacTask.setProcessors(Collections.emptyList());
    TestTaskListenerAdapter.addTaskListener(javacTask, tl1);
    TestTaskListenerAdapter.addTaskListener(javacTask, tl2);
    TestTaskListenerAdapter.addTaskListener(javacTask, tl3);

    javacTask.enter();

    assertThat(
        events,
        Matchers.contains(
            "1: started PARSE",
            "2: started PARSE",
            "3: started PARSE",
            "1: finished PARSE",
            "2: finished PARSE",
            "3: finished PARSE",
            "1: started ENTER",
            "3: started ENTER",
            "1: finished ENTER",
            "3: finished ENTER"));
  }

  @Test
  public void testSetTaskListenerDoesNotEraseAddedListeners() throws IOException {
    BuckJavacTask javacTask = testCompiler.getJavacTask();
    List<String> events = new ArrayList<>();

    RecordingTaskListener tl1 = new RecordingTaskListener("1", events);
    RecordingTaskListener tl2 = new RecordingTaskListener("2", events);
    RecordingTaskListener tl3 = new RecordingTaskListener("3", events);
    RecordingTaskListener tl4 = new RecordingTaskListener("4", events);

    javacTask.setProcessors(Collections.emptyList());
    TestTaskListenerAdapter.addTaskListener(javacTask, tl1);
    TestTaskListenerAdapter.setTaskListener(javacTask, tl4);
    TestTaskListenerAdapter.addTaskListener(javacTask, tl2);
    TestTaskListenerAdapter.setTaskListener(javacTask, tl3);
    javacTask.enter();

    assertThat(
        events,
        Matchers.contains(
            "1: started PARSE",
            "2: started PARSE",
            "3: started PARSE",
            "1: finished PARSE",
            "2: finished PARSE",
            "3: finished PARSE",
            "1: started ENTER",
            "2: started ENTER",
            "3: started ENTER",
            "1: finished ENTER",
            "2: finished ENTER",
            "3: finished ENTER"));
  }

  @Test
  public void testPluginsGetTheFirstEvent() throws IOException {
    BuckJavacTask javacTask = testCompiler.getJavacTask();
    List<String> events = new ArrayList<>();

    javacTask.addPlugin(
        new BuckJavacPlugin() {
          @Override
          public String getName() {
            return "Plugin!";
          }

          @Override
          public void init(BuckJavacTask task, String... args) {
            TestTaskListenerAdapter.setTaskListener(
                task, new RecordingTaskListener("Plugin", events));
          }
        });
    javacTask.setProcessors(Collections.emptyList());
    javacTask.enter();

    assertThat(
        events,
        Matchers.contains(
            "Plugin: started PARSE",
            "Plugin: finished PARSE",
            "Plugin: started ENTER",
            "Plugin: finished ENTER"));
  }

  static class RecordingTaskListener implements TestTaskListener {
    private final String name;
    private final List<String> events;

    public RecordingTaskListener(String name, List<String> events) {
      this.name = name;
      this.events = events;
    }

    @Override
    public void started(String event) {
      events.add(String.format("%s: started %s", name, event));
    }

    @Override
    public void finished(String event) {
      events.add(String.format("%s: finished %s", name, event));
    }
  }
}
