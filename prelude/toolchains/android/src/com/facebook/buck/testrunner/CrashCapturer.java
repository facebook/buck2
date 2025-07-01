/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.testrunner;

import com.android.ddmlib.IDevice;
import com.android.ddmlib.IShellOutputReceiver;
import com.android.ddmlib.MultiLineReceiver;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

/** Static methods to collect crash information from a device. */
public class CrashCapturer {
  /**
   * Checks if the given trace string may contain crash trace information.
   *
   * @param trace the trace string to check
   * @return true if the trace string ends with "Check device logcat for details", false otherwise
   */
  public static boolean deviceHasCrashLogs(String trace) {
    return trace.endsWith("Check device logcat for details");
  }

  /**
   * Adds device logcat trace lines to the given trace string.
   *
   * @param trace the trace string to add the logcat trace to
   * @return the trace string with the added logcat trace
   */
  public static String addDeviceLogcatTrace(IDevice device, String trace) {
    try {
      List<String> debugLines = new ArrayList<>();
      IShellOutputReceiver receiver =
          new MultiLineReceiver() {
            private boolean inDebuggerdDump = false;

            @Override
            public boolean isCancelled() {
              return false;
            }

            @Override
            public void processNewLines(String[] lines) {
              // Try to capture the last debuggerd dump in logcat.
              for (String line : lines) {
                if (!line.contains("DEBUG")) {
                  continue;
                }
                if (line.contains("*** *** ***")) {
                  debugLines.clear();
                  inDebuggerdDump = true;
                }
                if (inDebuggerdDump) {
                  debugLines.add(line);
                }
              }
            }
          };

      // Wait a short time for debuggerd to (hopefully) write some info out.
      Thread.sleep(1000);
      device.executeShellCommand("logcat -d", receiver, 10, TimeUnit.SECONDS);

      StringBuilder builder = new StringBuilder();
      builder.append(trace);
      if (debugLines.isEmpty()) {
        builder.append("\nSearched logcat, but unable to find a crash.\n");
      } else {
        builder.append("\nFound a crash in logcat:\n\n");
        for (String line : debugLines) {
          builder.append(line).append('\n');
        }
      }
      trace = builder.toString();
    } catch (Exception e) {
      // Is this too verbose?  Should we just ignore failure here?
      // Logging isn't configured, unfortunately.
      System.err.println("Warning: Failed to collect more info from logcat.");
      e.printStackTrace();
    }

    return trace;
  }
}
