/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.installer.apple;

import com.facebook.buck.android.device.TargetDeviceOptions;
import com.facebook.buck.installer.common.ConsumeAllOptionsHandler;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import javax.annotation.Nullable;
import org.kohsuke.args4j.Option;

/**
 * Constructs the Command Line Options to Support Apple Install. The majority of these were copied
 * from {@code com.facebook.buck.cli.TargetDeviceCommandLineOptions}.
 */
public class AppleCommandLineOptions {

  private static final String RUN_LONG_ARG = "--run";
  private static final String RUN_SHORT_ARG = "-r";

  @Option(
      name = RUN_LONG_ARG,
      aliases = {RUN_SHORT_ARG},
      usage = "To run an apple app")
  public boolean isRun = false;

  @Option(
      name = "--",
      usage = "Arguments passed when running with -r. Only valid for Apple targets.",
      handler = ConsumeAllOptionsHandler.class,
      depends = "-r")
  private List<String> runArgs = new ArrayList<>();

  private static final String WAIT_FOR_DEBUGGER_LONG_ARG = "--wait-for-debugger";
  private static final String WAIT_FOR_DEBUGGER_SHORT_ARG = "-w";

  @Option(
      name = WAIT_FOR_DEBUGGER_LONG_ARG,
      aliases = {WAIT_FOR_DEBUGGER_SHORT_ARG},
      usage = "Have the launched process wait for the debugger")
  private boolean waitForDebugger = false;

  private static final String DEVICE_MODE_SHORT_ARG = "-d";
  private static final String DEVICE_MODE_LONG_ARG = "--device";

  @Option(
      name = DEVICE_MODE_LONG_ARG,
      aliases = {DEVICE_MODE_SHORT_ARG},
      usage = "Use this option to use real devices only.")
  private boolean useRealDevicesOnlyMode;

  static final String SERIAL_NUMBER_SHORT_ARG = "-s";
  static final String SERIAL_NUMBER_LONG_ARG = "--serial";
  static final String UDID_ARG = "--udid";

  @Option(
      name = SERIAL_NUMBER_LONG_ARG,
      aliases = {SERIAL_NUMBER_SHORT_ARG, UDID_ARG},
      forbids = SIMULATOR_NAME_LONG_ARG,
      metaVar = "<serial-number>",
      usage = "Use device with specific serial or UDID number.")
  @Nullable
  private String serialNumber;

  static final String SIMULATOR_NAME_SHORT_ARG = "-n";
  static final String SIMULATOR_NAME_LONG_ARG = "--simulator-name";

  @Option(
      name = SIMULATOR_NAME_LONG_ARG,
      aliases = {SIMULATOR_NAME_SHORT_ARG},
      forbids = SERIAL_NUMBER_LONG_ARG,
      metaVar = "<name>",
      usage = "Use simulator with specific name (Apple only).")
  @Nullable
  private String simulatorName;

  @Option(
      name = "--tcp-port",
      usage = "TCP port used for connection in case TCP protocol is chosen")
  private int tcpPort = 50055;

  @Option(name = "--idb_path", usage = "Use this option to set the idb path for the install")
  private String idbPath = "/usr/local/bin/idb";

  @Option(name = "--log-path", usage = "Use this option to set the log path for the install")
  private String logPath;

  public AppleCommandLineOptions() {}

  public boolean getRun() {
    return isRun;
  }

  public List<String> getRunArgs() {
    return runArgs;
  }

  public boolean getWaitForDebugger() {
    return waitForDebugger;
  }

  public Optional<String> getSerialNumber() {
    return Optional.ofNullable(serialNumber);
  }

  public Optional<String> getSimulatorName() {
    return Optional.ofNullable(simulatorName);
  }

  public int getTcpPort() {
    return tcpPort;
  }

  public String getIdbPath() {
    return idbPath;
  }

  public String getLogPath() {
    return logPath;
  }

  /** Gets Target Device Options for Install */
  public TargetDeviceOptions getTargetDeviceOptions() {
    return new TargetDeviceOptions(false, useRealDevicesOnlyMode, getSerialNumber());
  }
}
