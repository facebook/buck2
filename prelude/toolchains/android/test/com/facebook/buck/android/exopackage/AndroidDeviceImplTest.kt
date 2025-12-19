/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.exopackage

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test
import org.mockito.kotlin.argThat
import org.mockito.kotlin.mock
import org.mockito.kotlin.never
import org.mockito.kotlin.verify
import org.mockito.kotlin.whenever

class AndroidDeviceImplTest {

  private lateinit var mockAdbUtils: AdbUtils
  private lateinit var androidDevice: AndroidDeviceImpl
  private val serialNumber = "test-serial"

  @Before
  fun setUp() {
    mockAdbUtils = mock()
    androidDevice = AndroidDeviceImpl(serialNumber, mockAdbUtils)
  }

  @Test
  fun testInstallApkOnDevice() {
    val apkFile = mock<File>()
    whenever(apkFile.absolutePath).thenReturn("/path/to/test.apk")
    whenever(apkFile.name).thenReturn("test.apk")
    whenever(apkFile.length()).thenReturn(1024L)
    whenever(mockAdbUtils.executeAdbShellCommand("getprop ro.build.version.sdk", serialNumber))
        .thenReturn("28")

    // Test with verifyTempWritable = true, SDK < 29 (no fastdeploy)
    val result = androidDevice.installApkOnDevice(apkFile, false, false, true, false)

    // Verify temp file check uses UUID pattern
    verify(mockAdbUtils)
        .executeAdbShellCommand(
            argThat { matches(Regex("echo exo > /data/local/tmp/buck-experiment-[0-9a-f\\-]+")) },
            org.mockito.kotlin.eq(serialNumber),
            org.mockito.kotlin.any(),
        )
    verify(mockAdbUtils)
        .executeAdbShellCommand(
            argThat { matches(Regex("rm /data/local/tmp/buck-experiment-[0-9a-f\\-]+")) },
            org.mockito.kotlin.eq(serialNumber),
            org.mockito.kotlin.any(),
        )

    verify(mockAdbUtils).executeAdbCommand("install -r -d ${apkFile.absolutePath}", serialNumber)
    assertTrue(result)
  }

  @Test
  fun testInstallApkOnDeviceWithInvalidSdkVersion() {
    val apkFile = mock<File>()
    whenever(apkFile.absolutePath).thenReturn("/path/to/test.apk")
    whenever(apkFile.name).thenReturn("test.apk")
    whenever(apkFile.length()).thenReturn(1024L)
    whenever(mockAdbUtils.executeAdbShellCommand("getprop ro.build.version.sdk", serialNumber))
        .thenReturn("invalid")

    // Test with invalid SDK version (should fall back to no fastdeploy)
    val result = androidDevice.installApkOnDevice(apkFile, false, false, false, false)

    verify(mockAdbUtils).executeAdbCommand("install -r -d ${apkFile.absolutePath}", serialNumber)
    assertTrue(result)
  }

  @Test
  fun testInstallApexOnDeviceWithRestart() {
    val apexFile = mock<File>()
    whenever(apexFile.absolutePath).thenReturn("/path/to/test.apex")
    whenever(apexFile.name).thenReturn("test.apex")
    whenever(apexFile.length()).thenReturn(1024L)

    // Mock boot completion check
    whenever(mockAdbUtils.executeAdbShellCommand("getprop sys.boot_completed", serialNumber, true))
        .thenReturn("1")
    // Mock package manager ready check
    whenever(mockAdbUtils.executeAdbShellCommand("pm", serialNumber, true))
        .thenReturn("Package manager is ready")
    // Mock storage ready check
    whenever(
            mockAdbUtils.executeAdbShellCommand(
                "ls /storage/emulated/0 2>&1 || echo STORAGE_NOT_READY",
                serialNumber,
                true,
            )
        )
        .thenReturn("Android\nDownload\nPictures")

    val result = androidDevice.installApexOnDevice(apexFile, false, true, true)

    verify(mockAdbUtils)
        .executeAdbCommand(
            "install --apex --force-non-staged ${apexFile.absolutePath}",
            serialNumber,
        )
    verify(mockAdbUtils).executeAdbShellCommand("stop", serialNumber)
    verify(mockAdbUtils).executeAdbShellCommand("start", serialNumber)
    verify(mockAdbUtils).executeAdbShellCommand("getprop sys.boot_completed", serialNumber, true)
    verify(mockAdbUtils).executeAdbShellCommand("pm", serialNumber, true)
    verify(mockAdbUtils)
        .executeAdbShellCommand(
            "ls /storage/emulated/0 2>&1 || echo STORAGE_NOT_READY",
            serialNumber,
            true,
        )
    assertTrue(result)
  }

  @Test
  fun testInstallApexOnDeviceWithoutRestart() {
    val apexFile = mock<File>()
    whenever(apexFile.absolutePath).thenReturn("/path/to/test.apex")
    whenever(apexFile.name).thenReturn("test.apex")
    whenever(apexFile.length()).thenReturn(1024L)

    val result = androidDevice.installApexOnDevice(apexFile, false, false, true)

    verify(mockAdbUtils)
        .executeAdbCommand(
            "install --apex --force-non-staged ${apexFile.absolutePath}",
            serialNumber,
        )
    // Verify that stop/start and boot checks are NOT called
    verify(mockAdbUtils, never()).executeAdbShellCommand("stop", serialNumber)
    verify(mockAdbUtils, never()).executeAdbShellCommand("start", serialNumber)
    verify(mockAdbUtils, never())
        .executeAdbShellCommand("getprop sys.boot_completed", serialNumber, true)
    assertTrue(result)
  }

  @Test
  fun testStopPackage() {
    androidDevice.stopPackage("com.test.app")

    verify(mockAdbUtils).executeAdbShellCommand("am force-stop com.test.app", serialNumber)
  }

  @Test
  fun testGetPackageInfo() {
    whenever(mockAdbUtils.executeAdbShellCommand("pm path com.test.app", serialNumber))
        .thenReturn("package:/data/app/com.test.app-1/base.apk")

    val result = androidDevice.getPackageInfo("com.test.app")

    assertTrue(result.isPresent)
    assertEquals("/data/app/com.test.app-1/base.apk", result.get().apkPath)
  }

  @Test
  fun testUninstallPackage() {
    androidDevice.uninstallPackage("com.test.app")

    verify(mockAdbUtils).executeAdbCommand("uninstall com.test.app", serialNumber)
  }

  @Test
  fun testGetSignature() {
    val packagePath = "/data/app/com.test.app-1/base.apk"
    whenever(
            mockAdbUtils.executeAdbShellCommand(
                "unzip -l $packagePath | grep -E -o 'META-INF/[A-Z]+\\.SF'",
                serialNumber,
            )
        )
        .thenReturn("META-INF/CERT.SF")
    whenever(
            mockAdbUtils.executeAdbShellCommand(
                "unzip -p $packagePath META-INF/CERT.SF | grep -E 'SHA1-Digest-Manifest:|SHA-256-Digest-Manifest:'",
                serialNumber,
            )
        )
        .thenReturn("SHA1-Digest-Manifest: abcdef1234567890")

    val result = androidDevice.getSignature(packagePath)

    assertEquals("abcdef1234567890", result)
  }

  @Test
  fun testGetSerialNumber() {
    assertEquals(serialNumber, androidDevice.getSerialNumber())
  }

  @Test
  fun testIsEmulator() {
    // Setup for non-emulator
    whenever(mockAdbUtils.executeAdbShellCommand("getprop ro.kernel.qemu", serialNumber))
        .thenReturn("0")

    assertFalse(androidDevice.isEmulator)

    // Setup for emulator
    whenever(mockAdbUtils.executeAdbShellCommand("getprop ro.kernel.qemu", serialNumber))
        .thenReturn("1")

    assertTrue(androidDevice.isEmulator)

    // Setup for Genymotion device
    whenever(mockAdbUtils.executeAdbShellCommand("getprop ro.kernel.qemu", serialNumber))
        .thenReturn("0")
    assertTrue(AndroidDeviceImpl("192.168.57.101:5555", mockAdbUtils).isEmulator)
  }

  @Test
  fun testGetDeviceAbis() {
    whenever(mockAdbUtils.executeAdbShellCommand("getprop ro.product.cpu.abilist", serialNumber))
        .thenReturn("arm64-v8a,armeabi-v7a,armeabi")

    val result = androidDevice.getDeviceAbis()

    assertEquals(listOf("arm64-v8a", "armeabi-v7a", "armeabi"), result)
  }

  @Test
  fun testGetDiskSpace() {
    whenever(
            mockAdbUtils.executeAdbShellCommand(
                "df -h /data | awk '{print $2, $3, $4}'",
                serialNumber,
            )
        )
        .thenReturn("Size Used Available\n64G 32G 32G")

    val result = androidDevice.getDiskSpace()

    assertEquals(listOf("64G", "32G", "32G"), result)
  }
}
