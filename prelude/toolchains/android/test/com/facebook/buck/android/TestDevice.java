/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android;

import com.android.ddmlib.Client;
import com.android.ddmlib.FileListingService;
import com.android.ddmlib.IDevice;
import com.android.ddmlib.IShellOutputReceiver;
import com.android.ddmlib.InstallException;
import com.android.ddmlib.InstallReceiver;
import com.android.ddmlib.RawImage;
import com.android.ddmlib.ScreenRecorderOptions;
import com.android.ddmlib.SyncService;
import com.android.ddmlib.log.LogReceiver;
import com.android.sdklib.AndroidVersion;
import com.google.common.util.concurrent.ListenableFuture;
import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/** Basic implementation of IDevice for mocking purposes. */
public class TestDevice implements IDevice {

  private boolean isEmulator;
  private String name;
  private String serialNumber;
  private DeviceState state;
  private final Map<String, String> properties;

  public static TestDevice createEmulator(String serial) {
    TestDevice device = new TestDevice();
    device.setIsEmulator(true);
    device.setSerialNumber(serial);
    device.setName("emulator-" + serial);
    device.setState(IDevice.DeviceState.ONLINE);
    return device;
  }

  public static TestDevice createRealDevice(String serial) {
    TestDevice device = new TestDevice();
    device.setIsEmulator(false);
    device.setSerialNumber(serial);
    device.setName("device-" + serial);
    device.setState(IDevice.DeviceState.ONLINE);
    return device;
  }

  public TestDevice() {
    properties = new HashMap<>();
  }

  public void setSerialNumber(String serialNumber) {
    this.serialNumber = serialNumber;
  }

  @Override
  public String getSerialNumber() {
    return serialNumber;
  }

  @Override
  public String getAvdName() {
    if (isEmulator()) {
      return name;
    } else {
      return null;
    }
  }

  @Deprecated
  public String getAvdPath() {
    return null;
  }

  public void setName(String name) {
    this.name = name;
  }

  @Override
  public String getName() {
    return name;
  }

  public void setIsEmulator(boolean isEmulator) {
    this.isEmulator = isEmulator;
  }

  @Override
  public boolean isEmulator() {
    return isEmulator;
  }

  public void setState(IDevice.DeviceState state) {
    this.state = state;
  }

  @Override
  public IDevice.DeviceState getState() {
    return state;
  }

  @Override
  public boolean isOnline() {
    return state == IDevice.DeviceState.ONLINE;
  }

  @Override
  public boolean isOffline() {
    return state == IDevice.DeviceState.OFFLINE;
  }

  @Override
  public boolean isBootLoader() {
    return state == IDevice.DeviceState.BOOTLOADER;
  }

  @Override
  public Map<String, String> getProperties() {
    return Collections.unmodifiableMap(properties);
  }

  @Override
  public int getPropertyCount() {
    return properties.size();
  }

  @Override
  public String getProperty(String s) {
    return properties.get(s);
  }

  @Override
  public boolean arePropertiesSet() {
    return true;
  }

  @Override
  public String getPropertySync(String s) {
    throw new UnsupportedOperationException();
  }

  @Override
  public String getPropertyCacheOrSync(String s) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean supportsFeature(IDevice.Feature feature) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean supportsFeature(IDevice.HardwareFeature feature) {
    return false;
  }

  @Override
  public String getMountPoint(String s) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean hasClients() {
    throw new UnsupportedOperationException();
  }

  @Override
  public Client[] getClients() {
    throw new UnsupportedOperationException();
  }

  @Override
  public Client getClient(String s) {
    throw new UnsupportedOperationException();
  }

  @Override
  public String getClientName(int i) {
    throw new UnsupportedOperationException();
  }

  @Override
  public SyncService getSyncService() {
    throw new UnsupportedOperationException();
  }

  @Override
  public FileListingService getFileListingService() {
    throw new UnsupportedOperationException();
  }

  @Override
  public RawImage getScreenshot() {
    throw new UnsupportedOperationException();
  }

  @Override
  public RawImage getScreenshot(long timeout, TimeUnit unit) {
    return null;
  }

  @Override
  public void startScreenRecorder(
      String remoteFilePath, ScreenRecorderOptions options, IShellOutputReceiver receiver) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void executeShellCommand(String s, IShellOutputReceiver iShellOutputReceiver) {
    throw new UnsupportedOperationException();
  }

  @Deprecated
  @Override
  public void executeShellCommand(String command, IShellOutputReceiver receiver, int i) {
    this.executeShellCommand(command, receiver);
  }

  @Override
  public void executeShellCommand(
      String command,
      IShellOutputReceiver receiver,
      long maxTimeToOutputResponse,
      TimeUnit maxTimeUnits) {
    this.executeShellCommand(command, receiver, 0);
  }

  @Override
  public void executeShellCommand(
      String command,
      IShellOutputReceiver receiver,
      long maxTimeToOutputResponse,
      long l,
      TimeUnit maxTimeUnits) {
    this.executeShellCommand(command, receiver, maxTimeToOutputResponse, maxTimeUnits);
  }

  @Override
  public ListenableFuture<String> getSystemProperty(String name) {
    return null;
  }

  @Override
  public void runEventLogService(LogReceiver logReceiver) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void runLogService(String s, LogReceiver logReceiver) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void createForward(int i, int i1) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void createForward(
      int i, String s, IDevice.DeviceUnixSocketNamespace deviceUnixSocketNamespace) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void removeForward(int i, int i1) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void removeForward(
      int i, String s, IDevice.DeviceUnixSocketNamespace deviceUnixSocketNamespace) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void pushFile(String s, String s1) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void pullFile(String s, String s1) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void installPackage(String s, boolean b, String... strings) throws InstallException {
    throw new UnsupportedOperationException();
  }

  @Override
  public void installPackage(
      String filePath, boolean reinstall, InstallReceiver receiver, String... extraArgs)
      throws InstallException {
    throw new UnsupportedOperationException();
  }

  @Override
  public void installPackage(
      String filePath,
      boolean reinstall,
      InstallReceiver receiver,
      long maxTimeout,
      long maxTimeToOutputResponse,
      TimeUnit maxTimeUnits,
      String... extraArgs)
      throws InstallException {
    throw new UnsupportedOperationException();
  }

  @Override
  public void installPackages(
      List<File> apks,
      boolean reinstall,
      List<String> installOptions,
      long timeout,
      TimeUnit timeoutUnit) {}

  @Override
  public String syncPackageToDevice(String s) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void installRemotePackage(String s, boolean b, String... strings) {
    throw new UnsupportedOperationException();
  }

  @Override
  public void installRemotePackage(
      String remoteFilePath, boolean reinstall, InstallReceiver receiver, String... extraArgs)
      throws InstallException {
    throw new UnsupportedOperationException();
  }

  @Override
  public void installRemotePackage(
      String remoteFilePath,
      boolean reinstall,
      InstallReceiver receiver,
      long maxTimeout,
      long maxTimeToOutputResponse,
      TimeUnit maxTimeUnits,
      String... extraArgs)
      throws InstallException {
    throw new UnsupportedOperationException();
  }

  @Override
  public void removeRemotePackage(String s) {
    throw new UnsupportedOperationException();
  }

  @Override
  public String uninstallPackage(String s) {
    throw new UnsupportedOperationException();
  }

  @Override
  public String uninstallApp(String applicationID, String... extraArgs) throws InstallException {
    throw new UnsupportedOperationException();
  }

  @Override
  public void reboot(String s) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean root() {
    return false;
  }

  @Override
  public boolean isRoot() {
    return false;
  }

  @Override
  public Integer getBatteryLevel() {
    throw new UnsupportedOperationException();
  }

  @Override
  public Integer getBatteryLevel(long l) {
    throw new UnsupportedOperationException();
  }

  @Override
  public ListenableFuture<Integer> getBattery() {
    return null;
  }

  @Override
  public ListenableFuture<Integer> getBattery(long freshnessTime, TimeUnit timeUnit) {
    return null;
  }

  @Override
  public List<String> getAbis() {
    return null;
  }

  @Override
  public int getDensity() {
    return 0;
  }

  @Override
  public String getLanguage() {
    return null;
  }

  @Override
  public String getRegion() {
    return null;
  }

  @Override
  public AndroidVersion getVersion() {
    return null;
  }
}
