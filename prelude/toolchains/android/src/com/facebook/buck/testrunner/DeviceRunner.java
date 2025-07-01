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

import com.android.ddmlib.AdbCommandRejectedException;
import com.android.ddmlib.AndroidDebugBridge;
import com.android.ddmlib.CollectingOutputReceiver;
import com.android.ddmlib.IDevice;
import com.android.ddmlib.ShellCommandUnresponsiveException;
import com.android.ddmlib.TimeoutException;
import com.google.common.annotations.VisibleForTesting;
import java.io.IOException;
import java.net.InetAddress;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class DeviceRunner {

  private static final long ADB_CONNECT_TIMEOUT_MS = 5000;
  private static final long ADB_CONNECT_TIME_STEP_MS = ADB_CONNECT_TIMEOUT_MS / 10;
  private static final String ADB_PATH = "ADB_PATH";
  private static final String PWD = "PWD";

  static class DeviceArgs {
    final boolean autoRunOnConnectedDevice;
    final String deviceSerial;
    final String adbExecutablePath;

    DeviceArgs(boolean autoRunOnConnectedDevice, String deviceSerial, String adbExecutablePath) {
      this.autoRunOnConnectedDevice = autoRunOnConnectedDevice;
      this.deviceSerial = deviceSerial;
      this.adbExecutablePath = adbExecutablePath;
    }
  }

  protected DeviceArgs deviceArgs;

  DeviceRunner(DeviceArgs args) {
    this.deviceArgs = args;
  }

  static DeviceArgs getDeviceArgs(String[] args) {
    String adbExecutablePath = null;
    boolean autoRunOnConnectedDevice = false;
    for (int i = 0; i < args.length; i++) {
      switch (args[i]) {
        case "--adb-executable-path":
          adbExecutablePath = args[++i];
          break;
        case "--auto-run-on-connected-device":
          autoRunOnConnectedDevice = true;
          break;
      }
    }

    if (adbExecutablePath == null) {
      System.err.println("Must pass --adb-executable-path argument.");
      System.exit(1);
    }

    final String adbPath = System.getenv(ADB_PATH);
    if (adbPath != null && !adbPath.isEmpty()) {
      adbExecutablePath = Paths.get(adbPath).toAbsolutePath().toString();
    }

    String buckdeviceSerial = System.getProperty("buck.device.id");
    String androidSerial = System.getenv("ANDROID_SERIAL");

    String deviceSerial = null;

    if (buckdeviceSerial != null) {
      deviceSerial = buckdeviceSerial;
    } else if (androidSerial != null) {
      deviceSerial = androidSerial;
    }

    if (deviceSerial != null) {
      // If a device serial was set, we use that instead of trying to auto connect.
      // This behavior is in line with other android tools.
      autoRunOnConnectedDevice = false;
    }

    if (deviceSerial == null && !autoRunOnConnectedDevice) {
      System.err.println(
          "Must pass buck.device.id system property, as this run is not configured to"
              + " auto-connect to device.");
      System.exit(1);
    }

    return new DeviceArgs(autoRunOnConnectedDevice, deviceSerial, adbExecutablePath);
  }

  /**
   * Method that either returns the given string as-is, or if the given string begins with a '@',
   * reads a single string from the specified file.
   */
  protected static String getArgValue(String input) throws IOException {
    if (!input.startsWith("@")) {
      return input;
    }

    Path inputPath = Paths.get(input.substring(1));
    return new String(Files.readAllBytes(inputPath));
  }

  protected String executeAdbShellCommand(String command, IDevice device)
      throws TimeoutException,
          AdbCommandRejectedException,
          ShellCommandUnresponsiveException,
          IOException {
    CollectingOutputReceiver receiver = new CollectingOutputReceiver();
    device.executeShellCommand(command, receiver);
    return receiver.getOutput();
  }

  @Nullable
  private AndroidDebugBridge getADB() throws InterruptedException {
    AndroidDebugBridge adb = createAdb();

    if (adb == null) {
      System.err.println("Unable to set up adb.");
      System.exit(1);
    }
    return adb;
  }

  /**
   * Creates connection to adb and waits for this connection to be initialized and receive initial
   * list of devices.
   */
  @Nullable
  @SuppressWarnings("PMD.EmptyCatchBlock")
  private AndroidDebugBridge createAdb() throws InterruptedException {
    AndroidDebugBridge.initIfNeeded(/* clientSupport */ false);
    AndroidDebugBridge adb = AndroidDebugBridge.createBridge(getAdbPath(), false);
    if (adb == null) {
      System.err.println("Failed to connect to adb. Make sure adb server is running.");
      return null;
    }

    long start = System.currentTimeMillis();
    while (!isAdbInitialized(adb)) {
      long timeLeft = start + ADB_CONNECT_TIMEOUT_MS - System.currentTimeMillis();
      if (timeLeft <= 0) {
        break;
      }
      Thread.sleep(ADB_CONNECT_TIME_STEP_MS);
    }
    return isAdbInitialized(adb) ? adb : null;
  }

  private boolean isAdbInitialized(AndroidDebugBridge adb) {
    return adb.isConnected() && adb.hasInitialDeviceList();
  }

  @Nullable
  private IDevice getDevice(String serial) throws InterruptedException {
    AndroidDebugBridge adb = getADB();

    IDevice[] allDevices = adb.getDevices();
    for (IDevice device : allDevices) {
      if (device.getSerialNumber().equals(serial)) {
        return device;
      }
    }
    return null;
  }

  @Nullable
  private IDevice getFirstConnectedDevice() throws InterruptedException {
    AndroidDebugBridge adb = getADB();

    IDevice[] allDevices = adb.getDevices();
    if (allDevices.length == 0) {
      String hostname = null;
      try {
        hostname = InetAddress.getLocalHost().getHostName();
      } catch (Exception _e) {
        // no op
      }
      System.err.println(
          String.format(
              "Found 0 device %s-- please connect a device.",
              hostname == null ? "" : String.format("on %s ", hostname)));
      System.exit(1);
    }
    return allDevices[0];
  }

  @Nullable
  @VisibleForTesting
  IDevice getAndroidDevice(boolean autoRunOnConnectedDevice, String deviceSerial)
      throws InterruptedException {
    IDevice device = null;
    if (autoRunOnConnectedDevice) {
      device = getFirstConnectedDevice();
    } else {
      device = getDevice(deviceSerial);
      if (device == null) {
        System.err.printf("Unable to get device/emulator with serial %s", deviceSerial);
        System.exit(1);
      }
    }

    return device;
  }

  protected String getAdbPath() {
    return deviceArgs.adbExecutablePath;
  }

  /** We minimize external dependencies, but we'd like to have {@link javax.annotation.Nullable}. */
  @interface Nullable {}
}
