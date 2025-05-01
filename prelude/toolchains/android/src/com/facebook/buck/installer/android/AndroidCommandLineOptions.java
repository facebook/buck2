/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.installer.android;

import com.google.common.annotations.VisibleForTesting;
import javax.annotation.Nullable;
import org.kohsuke.args4j.Option;

/**
 * Constructs the Command Line Options to Support Android Install. The majority of these were copied
 * from {@code com.facebook.buck.cli.TargetDeviceCommandLineOptions}.
 */
class AndroidCommandLineOptions {
  @VisibleForTesting public static final String EMULATOR_MODE_SHORT_ARG = "-e";

  @VisibleForTesting static final String EMULATOR_MODE_LONG_ARG = "--emulator";

  @Option(
      name = EMULATOR_MODE_LONG_ARG,
      aliases = {EMULATOR_MODE_SHORT_ARG},
      usage = "Use this option to use emulators only.")
  public boolean useEmulatorsOnlyMode;

  @VisibleForTesting static final String DEVICE_MODE_SHORT_ARG = "-d";
  @VisibleForTesting static final String DEVICE_MODE_LONG_ARG = "--device";

  @Option(
      name = DEVICE_MODE_LONG_ARG,
      aliases = {DEVICE_MODE_SHORT_ARG},
      usage = "Use this option to use real devices only.")
  public boolean useRealDevicesOnlyMode;

  @VisibleForTesting static final String SERIAL_NUMBER_SHORT_ARG = "-s";
  @VisibleForTesting static final String SERIAL_NUMBER_LONG_ARG = "--serial";
  static final String UDID_ARG = "--udid";

  @Option(
      name = SERIAL_NUMBER_LONG_ARG,
      aliases = {SERIAL_NUMBER_SHORT_ARG, UDID_ARG},
      metaVar = "<serial-number>",
      usage = "Use device or emulator with specific serial or UDID number.")
  @Nullable
  public String serialNumber;

  @VisibleForTesting static final String ADB_THREADS_LONG_ARG = "--adb-threads";
  @VisibleForTesting static final String ADB_THREADS_SHORT_ARG = "-T";

  @Option(
      name = ADB_THREADS_LONG_ARG,
      aliases = {ADB_THREADS_SHORT_ARG},
      usage =
          "Number of threads to use for adb operations. "
              + "Defaults to number of connected devices.")
  public int adbThreadCount = 0;

  static final String ADB_SERVER_PORT_LONG_ARG = "--adb-server-port";

  @Option(
      name = ADB_SERVER_PORT_LONG_ARG,
      metaVar = "<TCP-port>",
      usage = "Use ADB server with specific TCP port.")
  public int adbServerPort = 0;

  @VisibleForTesting static final String MULTI_INSTALL_MODE_SHORT_ARG = "-x";
  @VisibleForTesting static final String MULTI_INSTALL_MODE_LONG_ARG = "--all-devices";

  @Option(
      name = MULTI_INSTALL_MODE_LONG_ARG,
      aliases = {MULTI_INSTALL_MODE_SHORT_ARG},
      usage = "Install .apk on all connected devices and/or emulators (multi-install mode)")
  public boolean multiInstallMode;

  public final int adbTimeout = 60_000;

  @Option(
      name = "--tcp-port",
      usage = "TCP port used for connection in case TCP protocol is chosen")
  private int tcpPort;

  @Option(name = "--log-path", usage = "Use this option to set the log path for the install")
  private String logPath;

  @Option(
      name = "--run",
      aliases = {"-r"},
      usage = "Run an activity (the default activity for package unless -a is specified).")
  public boolean run = false;

  @Option(
      name = "--activity",
      aliases = {"-a"},
      metaVar = "<pkg/activity>",
      usage = "Activity to launch e.g. com.facebook.katana/.LoginActivity. Implies -r.")
  @Nullable
  public String activity = null;

  @Option(
      name = "--intent-uri",
      aliases = {"-i"},
      metaVar = "<pkg/intent_uri>",
      usage = "Intent URI to launch e.g. fb://profile. Implies -r.")
  @Nullable
  public String intentUri = null;

  @Option(
      name = "--wait-for-debugger",
      aliases = {"-w"},
      usage = "Have the launched process wait for the debugger")
  public boolean waitForDebugger = false;

  @Option(
      name = "--skip-set-debug-app",
      aliases = {"-b"},
      usage =
          "Bypass default invocation of Android's set-debug-app which silences ANRs while running"
              + " and debugging.")
  public boolean skipSetDebugApp = false;

  @Option(
      name = "--clean-up",
      aliases = {"-c"},
      usage = "Cleans up installed app.")
  public boolean cleanUp = false;

  @Option(
      name = "--keep",
      aliases = {"-k"},
      usage = "Keeps user data when uninstalling an app")
  public boolean keepUserData = false;

  @Option(
      name = "--uninstall",
      aliases = {"-u"},
      usage = "Uninstall before installing an app")
  public boolean uninstallFirst = false;

  @VisibleForTesting
  static final String IGNORE_MISSING_DEVICES_LONG_ARG = "--ignore-missing-device";

  @VisibleForTesting static final String IGNORE_MISSING_DEVICES_SHORT_ARG = "-m";

  @Option(
      name = IGNORE_MISSING_DEVICES_LONG_ARG,
      aliases = {IGNORE_MISSING_DEVICES_SHORT_ARG},
      usage = "Do not exit with nonzero, even if ADB detects no devices connected.")
  public boolean ignoreMissingDevices = false;

  public AndroidCommandLineOptions() {}

  public int getTcpPort() {
    return tcpPort;
  }

  public String getLogPath() {
    return logPath;
  }
}
