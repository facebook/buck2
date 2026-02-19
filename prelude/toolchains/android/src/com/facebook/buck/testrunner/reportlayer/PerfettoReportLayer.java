/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.testrunner.reportlayer;

import com.facebook.buck.testrunner.InstrumentationTestRunner;
import java.nio.file.Files;
import java.nio.file.Path;

/** Report layer to collect Perfetto system traces during instrumentation tests. */
public class PerfettoReportLayer extends ReportLayer {

  public static final String ARG = "--collect-perfetto";

  private static final String PERFETTO_TRACE_DEVICE_DIR = "/data/misc/perfetto-traces/";
  private static final String PERFETTO_TRACE_FILENAME = "ait_perfetto_trace.perfetto-trace";

  // Default Perfetto config: ftrace events (sched, cpu freq/idle, wakeup),
  // process_stats, sys_stats, 64KB buffer with RING_BUFFER, 600s max duration, 100MB max file size
  private static final String PERFETTO_CONFIG =
      "buffers { size_kb: 65536 fill_policy: RING_BUFFER } "
          + "data_sources { config { name: \"linux.ftrace\" "
          + "ftrace_config { ftrace_events: \"sched/sched_switch\" "
          + "ftrace_events: \"power/cpu_frequency\" "
          + "ftrace_events: \"power/cpu_idle\" "
          + "ftrace_events: \"sched/sched_wakeup\" } } } "
          + "data_sources { config { name: \"linux.process_stats\" } } "
          + "data_sources { config { name: \"linux.sys_stats\" } } "
          + "duration_ms: 600000 "
          + "max_file_size_bytes: 104857600";

  private String perfettoPid = null;

  public PerfettoReportLayer(InstrumentationTestRunner runner) {
    super(runner);
  }

  @Override
  public void initialize() {
    try {
      String tracePath = PERFETTO_TRACE_DEVICE_DIR + PERFETTO_TRACE_FILENAME;

      // Clean up any existing trace file
      this.runner.runShellCommand("rm -f " + tracePath);

      // Start perfetto in background mode with text config via stdin
      String startCmd =
          String.format(
              "echo '%s' | perfetto --txt --background --config - --out %s",
              PERFETTO_CONFIG, tracePath);
      String output = this.runner.runShellCommand(startCmd);

      // Extract PID from perfetto output (format: "[NNN]")
      if (output != null && output.contains("[")) {
        int start = output.indexOf('[') + 1;
        int end = output.indexOf(']');
        if (end > start) {
          perfettoPid = output.substring(start, end).trim();
          System.err.printf("Perfetto started with PID: %s%n", perfettoPid);
        }
      }

      if (perfettoPid == null) {
        System.err.println("Warning: Could not determine Perfetto PID from output: " + output);
      }
    } catch (Exception e) {
      System.err.printf("Failed to start Perfetto tracing: %s%n", e);
    }
  }

  @Override
  public void report() {
    try {
      String tracePath = PERFETTO_TRACE_DEVICE_DIR + PERFETTO_TRACE_FILENAME;

      // Stop perfetto gracefully via SIGTERM
      if (perfettoPid != null) {
        this.runner.runShellCommand("kill -TERM " + perfettoPid);
        // Wait for perfetto to flush and finalize the trace
        Thread.sleep(3000);
      }

      Path artifact =
          this.runner.createTRA("generic_blob", "Perfetto trace", PERFETTO_TRACE_FILENAME);
      if (artifact == null) {
        System.err.println("Cannot report Perfetto trace: TRA directory not configured");
        return;
      }

      this.runner.pullFileWithSyncService(tracePath, artifact.toString());

      if (!Files.exists(artifact) || Files.size(artifact) == 0) {
        System.err.println("Warning: Perfetto trace file is empty or missing");
      }

      // Clean up trace from device
      this.runner.runShellCommand("rm -f " + tracePath);
    } catch (Exception e) {
      System.err.printf("Failed to collect Perfetto trace: %s%n", e);
    }
  }
}
