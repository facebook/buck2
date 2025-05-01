/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android

import com.android.ddmlib.AdbCommandRejectedException
import com.android.ddmlib.AvdData
import com.android.ddmlib.Client
import com.android.ddmlib.FileListingService
import com.android.ddmlib.IDevice
import com.android.ddmlib.IDevice.DeviceState
import com.android.ddmlib.IDevice.DeviceUnixSocketNamespace
import com.android.ddmlib.IDevice.HardwareFeature
import com.android.ddmlib.IShellOutputReceiver
import com.android.ddmlib.InstallException
import com.android.ddmlib.InstallMetrics
import com.android.ddmlib.InstallReceiver
import com.android.ddmlib.ProfileableClient
import com.android.ddmlib.RawImage
import com.android.ddmlib.ScreenRecorderOptions
import com.android.ddmlib.ShellCommandUnresponsiveException
import com.android.ddmlib.SyncException
import com.android.ddmlib.SyncService
import com.android.ddmlib.TimeoutException
import com.android.ddmlib.clientmanager.DeviceClientManager
import com.android.ddmlib.log.LogReceiver
import com.android.sdklib.AndroidVersion
import com.google.common.util.concurrent.ListenableFuture
import java.io.File
import java.io.IOException
import java.io.InputStream
import java.nio.channels.SocketChannel
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit

class RetryingAndroidDevice
internal constructor(
    private val device: IDevice,
    private val maxRetries: Int,
    private val retryDelayMs: Long
) : IDevice {
  override fun getSerialNumber(): String {
    return withRetries { device.serialNumber }
  }

  @Deprecated("")
  override fun getAvdName(): String? {
    return withRetries { device.avdName }
  }

  @Deprecated("")
  override fun getAvdPath(): String {
    return withRetries { device.avdPath }
  }

  override fun getAvdData(): ListenableFuture<AvdData> {
    return withRetries { device.avdData }
  }

  override fun getState(): DeviceState {
    return withRetries { device.state }
  }

  @Deprecated("")
  override fun getProperties(): Map<String, String> {
    return withRetries { device.properties }
  }

  @Deprecated("")
  override fun getPropertyCount(): Int {
    return withRetries { device.propertyCount }
  }

  override fun getProperty(name: String): String? {
    return withRetries { device.getProperty(name) }
  }

  override fun arePropertiesSet(): Boolean {
    return withRetries { device.arePropertiesSet() }
  }

  @Deprecated("")
  @Throws(
      TimeoutException::class,
      AdbCommandRejectedException::class,
      ShellCommandUnresponsiveException::class,
      IOException::class)
  override fun getPropertySync(name: String): String {
    return withRetries { device.getPropertySync(name) }
  }

  @Deprecated("")
  @Throws(
      TimeoutException::class,
      AdbCommandRejectedException::class,
      ShellCommandUnresponsiveException::class,
      IOException::class)
  override fun getPropertyCacheOrSync(name: String): String {
    return withRetries { device.getPropertyCacheOrSync(name) }
  }

  override fun supportsFeature(feature: IDevice.Feature): Boolean {
    return withRetries { device.supportsFeature(feature) }
  }

  override fun supportsFeature(feature: HardwareFeature): Boolean {
    return withRetries { device.supportsFeature(feature) }
  }

  override fun getMountPoint(name: String): String {
    return withRetries { device.getMountPoint(name) }
  }

  override fun isOnline(): Boolean {
    return withRetries { device.isOnline }
  }

  override fun isEmulator(): Boolean {
    return withRetries { device.isEmulator }
  }

  override fun isOffline(): Boolean {
    return withRetries { device.isOffline }
  }

  override fun isBootLoader(): Boolean {
    return withRetries { device.isBootLoader }
  }

  override fun hasClients(): Boolean {
    return withRetries { device.hasClients() }
  }

  override fun getClients(): Array<Client> {
    return withRetries { device.clients }
  }

  override fun getClient(applicationName: String): Client {
    return withRetries { device.getClient(applicationName) }
  }

  override fun getProfileableClients(): Array<ProfileableClient> {
    return withRetries { device.profileableClients }
  }

  override fun getClientManager(): DeviceClientManager {
    return withRetries { device.clientManager }
  }

  override fun forceStop(applicationName: String) {
    withRetries { device.forceStop(applicationName) }
  }

  override fun kill(applicationName: String) {
    withRetries { device.kill(applicationName) }
  }

  @Throws(TimeoutException::class, AdbCommandRejectedException::class, IOException::class)
  override fun getSyncService(): SyncService {
    return withRetries { device.syncService }
  }

  override fun getFileListingService(): FileListingService {
    return withRetries { device.fileListingService }
  }

  @Throws(TimeoutException::class, AdbCommandRejectedException::class, IOException::class)
  override fun getScreenshot(): RawImage {
    return withRetries { device.screenshot }
  }

  @Throws(TimeoutException::class, AdbCommandRejectedException::class, IOException::class)
  override fun getScreenshot(timeout: Long, unit: TimeUnit): RawImage {
    return withRetries { device.getScreenshot(timeout, unit) }
  }

  @Throws(
      TimeoutException::class,
      AdbCommandRejectedException::class,
      IOException::class,
      ShellCommandUnresponsiveException::class)
  override fun startScreenRecorder(
      remoteFilePath: String,
      options: ScreenRecorderOptions,
      receiver: IShellOutputReceiver
  ) {
    withRetries { device.startScreenRecorder(remoteFilePath, options, receiver) }
  }

  @Deprecated("")
  @Throws(
      TimeoutException::class,
      AdbCommandRejectedException::class,
      ShellCommandUnresponsiveException::class,
      IOException::class)
  override fun executeShellCommand(
      command: String,
      receiver: IShellOutputReceiver,
      maxTimeToOutputResponse: Int
  ) {
    withRetries { device.executeShellCommand(command, receiver, maxTimeToOutputResponse) }
  }

  @Throws(
      TimeoutException::class,
      AdbCommandRejectedException::class,
      ShellCommandUnresponsiveException::class,
      IOException::class)
  override fun executeShellCommand(command: String, receiver: IShellOutputReceiver) {
    withRetries { device.executeShellCommand(command, receiver) }
  }

  @Throws(
      TimeoutException::class,
      AdbCommandRejectedException::class,
      ShellCommandUnresponsiveException::class,
      IOException::class)
  override fun executeShellCommand(
      command: String,
      receiver: IShellOutputReceiver,
      maxTimeToOutputResponse: Long,
      maxTimeUnits: TimeUnit,
      `is`: InputStream
  ) {
    withRetries {
      device.executeShellCommand(command, receiver, maxTimeToOutputResponse, maxTimeUnits, `is`)
    }
  }

  @Throws(
      TimeoutException::class,
      AdbCommandRejectedException::class,
      ShellCommandUnresponsiveException::class,
      IOException::class)
  override fun executeBinderCommand(
      parameters: Array<String>,
      receiver: IShellOutputReceiver,
      maxTimeToOutputResponse: Long,
      maxTimeUnits: TimeUnit,
      `is`: InputStream
  ) {
    withRetries {
      device.executeBinderCommand(parameters, receiver, maxTimeToOutputResponse, maxTimeUnits, `is`)
    }
  }

  @Throws(TimeoutException::class, AdbCommandRejectedException::class, IOException::class)
  override fun runEventLogService(receiver: LogReceiver) {
    withRetries { device.runEventLogService(receiver) }
  }

  @Throws(TimeoutException::class, AdbCommandRejectedException::class, IOException::class)
  override fun runLogService(logname: String, receiver: LogReceiver) {
    withRetries { device.runLogService(logname, receiver) }
  }

  @Throws(TimeoutException::class, AdbCommandRejectedException::class, IOException::class)
  override fun createForward(localPort: Int, remotePort: Int) {
    withRetries { device.createForward(localPort, remotePort) }
  }

  @Throws(TimeoutException::class, AdbCommandRejectedException::class, IOException::class)
  override fun createForward(
      localPort: Int,
      remoteSocketName: String,
      namespace: DeviceUnixSocketNamespace
  ) {
    withRetries { device.createForward(localPort, remoteSocketName, namespace) }
  }

  @Throws(TimeoutException::class, AdbCommandRejectedException::class, IOException::class)
  override fun removeForward(localPort: Int) {
    withRetries { device.removeForward(localPort) }
  }

  @Deprecated("")
  @Throws(TimeoutException::class, AdbCommandRejectedException::class, IOException::class)
  override fun removeForward(localPort: Int, remotePort: Int) {
    withRetries { device.removeForward(localPort, remotePort) }
  }

  @Deprecated("")
  @Throws(TimeoutException::class, AdbCommandRejectedException::class, IOException::class)
  override fun removeForward(
      localPort: Int,
      remoteSocketName: String,
      namespace: DeviceUnixSocketNamespace
  ) {
    withRetries { device.removeForward(localPort, remoteSocketName, namespace) }
  }

  @Throws(TimeoutException::class, AdbCommandRejectedException::class, IOException::class)
  override fun createReverse(remotePort: Int, localPort: Int) {
    withRetries { device.createReverse(remotePort, localPort) }
  }

  @Throws(TimeoutException::class, AdbCommandRejectedException::class, IOException::class)
  override fun removeReverse(remotePort: Int) {
    withRetries { device.removeReverse(remotePort) }
  }

  override fun getClientName(pid: Int): String {
    return withRetries { device.getClientName(pid) }
  }

  @Throws(
      IOException::class,
      AdbCommandRejectedException::class,
      TimeoutException::class,
      SyncException::class)
  override fun push(local: Array<String>, remote: String) {
    withRetries { device.push(local, remote) }
  }

  @Throws(
      IOException::class,
      AdbCommandRejectedException::class,
      TimeoutException::class,
      SyncException::class)
  override fun pushFile(local: String, remote: String) {
    withRetries { device.pushFile(local, remote) }
  }

  @Throws(
      IOException::class,
      AdbCommandRejectedException::class,
      TimeoutException::class,
      SyncException::class)
  override fun pullFile(remote: String, local: String) {
    withRetries { device.pullFile(remote, local) }
  }

  @Throws(InstallException::class)
  override fun installPackage(
      packageFilePath: String,
      reinstall: Boolean,
      vararg extraArgs: String
  ) {
    withRetries { device.installPackage(packageFilePath, reinstall, *extraArgs) }
  }

  @Throws(InstallException::class)
  override fun installPackage(
      packageFilePath: String,
      reinstall: Boolean,
      receiver: InstallReceiver,
      vararg extraArgs: String
  ) {
    withRetries { device.installPackage(packageFilePath, reinstall, receiver, *extraArgs) }
  }

  @Throws(InstallException::class)
  override fun installPackage(
      packageFilePath: String,
      reinstall: Boolean,
      receiver: InstallReceiver,
      maxTimeout: Long,
      maxTimeToOutputResponse: Long,
      maxTimeUnits: TimeUnit,
      vararg extraArgs: String
  ) {
    withRetries {
      device.installPackage(
          packageFilePath,
          reinstall,
          receiver,
          maxTimeout,
          maxTimeToOutputResponse,
          maxTimeUnits,
          *extraArgs)
    }
  }

  @Throws(InstallException::class)
  override fun installPackages(
      apks: List<File>,
      reinstall: Boolean,
      installOptions: List<String>,
      timeout: Long,
      timeoutUnit: TimeUnit
  ) {
    withRetries { device.installPackages(apks, reinstall, installOptions, timeout, timeoutUnit) }
  }

  @Throws(InstallException::class)
  override fun installPackages(apks: List<File>, reinstall: Boolean, installOptions: List<String>) {
    withRetries { device.installPackages(apks, reinstall, installOptions) }
  }

  override fun getLastInstallMetrics(): InstallMetrics {
    return withRetries { device.lastInstallMetrics }
  }

  @Throws(InstallException::class)
  override fun installRemotePackages(
      remoteApks: List<String>,
      reinstall: Boolean,
      installOptions: List<String>,
      timeout: Long,
      timeoutUnit: TimeUnit
  ) {
    withRetries {
      device.installRemotePackages(remoteApks, reinstall, installOptions, timeout, timeoutUnit)
    }
  }

  @Throws(InstallException::class)
  override fun installRemotePackages(
      remoteApks: List<String>,
      reinstall: Boolean,
      installOptions: List<String>
  ) {
    withRetries { device.installRemotePackages(remoteApks, reinstall, installOptions) }
  }

  @Throws(
      TimeoutException::class,
      AdbCommandRejectedException::class,
      IOException::class,
      SyncException::class)
  override fun syncPackageToDevice(localFilePath: String): String {
    return withRetries { device.syncPackageToDevice(localFilePath) }
  }

  @Throws(InstallException::class)
  override fun installRemotePackage(
      remoteFilePath: String,
      reinstall: Boolean,
      vararg extraArgs: String
  ) {
    withRetries { device.installRemotePackage(remoteFilePath, reinstall, *extraArgs) }
  }

  @Throws(InstallException::class)
  override fun installRemotePackage(
      remoteFilePath: String,
      reinstall: Boolean,
      receiver: InstallReceiver,
      vararg extraArgs: String
  ) {
    withRetries { device.installRemotePackage(remoteFilePath, reinstall, receiver, *extraArgs) }
  }

  @Throws(InstallException::class)
  override fun installRemotePackage(
      remoteFilePath: String,
      reinstall: Boolean,
      receiver: InstallReceiver,
      maxTimeout: Long,
      maxTimeToOutputResponse: Long,
      maxTimeUnits: TimeUnit,
      vararg extraArgs: String
  ) {
    withRetries {
      device.installRemotePackage(
          remoteFilePath,
          reinstall,
          receiver,
          maxTimeout,
          maxTimeToOutputResponse,
          maxTimeUnits,
          *extraArgs)
    }
  }

  @Throws(InstallException::class)
  override fun removeRemotePackage(remoteFilePath: String) {
    withRetries { device.removeRemotePackage(remoteFilePath) }
  }

  @Throws(InstallException::class)
  override fun uninstallPackage(packageName: String): String {
    return device.uninstallPackage(packageName)
  }

  @Throws(InstallException::class)
  override fun uninstallApp(applicationID: String, vararg extraArgs: String): String {
    return device.uninstallApp(applicationID, *extraArgs)
  }

  @Throws(TimeoutException::class, AdbCommandRejectedException::class, IOException::class)
  override fun reboot(into: String?) {
    device.reboot(into)
  }

  @Throws(
      TimeoutException::class,
      AdbCommandRejectedException::class,
      IOException::class,
      ShellCommandUnresponsiveException::class)
  override fun root(): Boolean {
    return withRetries { device.root() }
  }

  @Throws(
      TimeoutException::class,
      AdbCommandRejectedException::class,
      IOException::class,
      ShellCommandUnresponsiveException::class)
  override fun isRoot(): Boolean {
    return withRetries { device.isRoot }
  }

  @Deprecated("")
  @Throws(
      TimeoutException::class,
      AdbCommandRejectedException::class,
      IOException::class,
      ShellCommandUnresponsiveException::class)
  override fun getBatteryLevel(): Int {
    return withRetries { device.batteryLevel }
  }

  @Deprecated("")
  @Throws(
      TimeoutException::class,
      AdbCommandRejectedException::class,
      IOException::class,
      ShellCommandUnresponsiveException::class)
  override fun getBatteryLevel(freshnessMs: Long): Int {
    return withRetries { device.getBatteryLevel(freshnessMs) }
  }

  override fun getBattery(): Future<Int> {
    return withRetries { device.battery }
  }

  override fun getBattery(freshnessTime: Long, timeUnit: TimeUnit): Future<Int> {
    return withRetries { device.getBattery(freshnessTime, timeUnit) }
  }

  override fun getAbis(): List<String> {
    return withRetries { device.abis }
  }

  override fun getDensity(): Int {
    return withRetries { device.density }
  }

  override fun getLanguage(): String {
    return withRetries { device.language }
  }

  override fun getRegion(): String {
    return withRetries { device.region }
  }

  override fun getVersion(): AndroidVersion {
    return withRetries { device.version }
  }

  @Throws(AdbCommandRejectedException::class, TimeoutException::class, IOException::class)
  override fun rawExec(executable: String, parameters: Array<String>): SocketChannel {
    return withRetries { device.rawExec(executable, parameters) }
  }

  @Throws(AdbCommandRejectedException::class, TimeoutException::class, IOException::class)
  override fun rawBinder(service: String, parameters: Array<String>): SocketChannel {
    return withRetries { device.rawBinder(service, parameters) }
  }

  @Throws(Exception::class)
  override fun getHardwareCharacteristics(): Set<String> {
    return withRetries { device.hardwareCharacteristics }
  }

  override fun getName(): String {
    return withRetries { device.name }
  }

  @Throws(
      TimeoutException::class,
      AdbCommandRejectedException::class,
      ShellCommandUnresponsiveException::class,
      IOException::class)
  override fun executeShellCommand(
      command: String,
      receiver: IShellOutputReceiver,
      maxTimeToOutputResponse: Long,
      maxTimeUnits: TimeUnit
  ) {
    withRetries {
      device.executeShellCommand(command, receiver, maxTimeToOutputResponse, maxTimeUnits)
    }
  }

  @Throws(
      TimeoutException::class,
      AdbCommandRejectedException::class,
      ShellCommandUnresponsiveException::class,
      IOException::class)
  override fun executeShellCommand(
      command: String,
      receiver: IShellOutputReceiver,
      maxTimeout: Long,
      maxTimeToOutputResponse: Long,
      maxTimeUnits: TimeUnit
  ) {
    withRetries {
      device.executeShellCommand(
          command, receiver, maxTimeout, maxTimeToOutputResponse, maxTimeUnits)
    }
  }

  override fun getSystemProperty(name: String): ListenableFuture<String> {
    return withRetries { device.getSystemProperty(name) }
  }

  private inline fun <T> withRetries(action: () -> T): T {
    var retries = 0
    while (true) {
      try {
        return action()
      } catch (e: Throwable) {
        if (retries >= maxRetries) {
          throw e
        } else {
          Thread.sleep(retryDelayMs)
          retries++
        }
      }
    }
  }
}
