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

import com.android.ddmlib.AndroidDebugBridge;
import com.android.ddmlib.IDevice;
import com.facebook.buck.android.exopackage.AdbUtils;
import com.google.common.annotations.VisibleForTesting;
import java.io.IOException;
import java.net.InetAddress;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class DeviceRunner {

  private static final long ADB_CONNECT_TIMEOUT_MS = 30000;
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
  private IDevice findDeviceInBridge(AndroidDebugBridge adb, String serial) {
    IDevice[] allDevices = adb.getDevices();
    for (IDevice device : allDevices) {
      if (device.getSerialNumber().equals(serial)) {
        return device;
      }
    }
    return null;
  }

  /**
   * Returns true if the serial looks like a TCP-connected device (host:port format), which is
   * typical for emulators connected via {@code adb connect}.
   */
  private static boolean isTcpDevice(String serial) {
    return serial != null && serial.contains(":");
  }

  /**
   * Runs {@code adb connect <serial>} to ensure the ADB server managed by ddmlib knows about a
   * TCP-connected device (e.g., an emulator). This is necessary because the ddmlib bridge may
   * connect to a different ADB server instance than the one that originally had the device
   * connected.
   */
  private void connectTcpDevice(String serial) {
    try {
      AdbUtils adbUtils = new AdbUtils(getAdbPath(), 0);
      String result = adbUtils.executeAdbCommand("connect " + serial, null, true);
      System.err.println("adb connect " + serial + ": " + result.trim());
    } catch (Exception e) {
      System.err.println("Failed to run adb connect " + serial + ": " + e.getMessage());
    }
  }

  @Nullable
  private IDevice getDevice(String serial) throws InterruptedException {
    AndroidDebugBridge adb = getADB();

    IDevice device = findDeviceInBridge(adb, serial);
    if (device != null) {
      return device;
    }

    // For TCP-connected devices (e.g., emulators at host:port), the ddmlib bridge may be
    // connected to an ADB server that doesn't know about this device. Run "adb connect" to
    // register the device with the ADB server, then wait for ddmlib to pick it up.
    if (isTcpDevice(serial)) {
      System.err.println("Device " + serial + " not found via ddmlib, attempting adb connect...");
      connectTcpDevice(serial);

      // Wait for ddmlib to discover the newly-connected device
      long start = System.currentTimeMillis();
      while (device == null) {
        long timeLeft = start + ADB_CONNECT_TIMEOUT_MS - System.currentTimeMillis();
        if (timeLeft <= 0) {
          break;
        }
        Thread.sleep(ADB_CONNECT_TIME_STEP_MS);
        device = findDeviceInBridge(adb, serial);
      }
    }

    return device;
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
