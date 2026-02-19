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

import com.facebook.buck.core.util.log.Logger
import com.facebook.buck.installer.android.AndroidInstallException
import com.google.common.base.Splitter
import com.google.common.collect.ImmutableSortedSet
import com.google.common.collect.Sets
import java.io.File
import java.lang.Thread.sleep
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardCopyOption
import java.util.Optional
import java.util.UUID
import java.util.regex.Pattern
import kotlin.system.measureTimeMillis

class AndroidDeviceImpl(val serial: String, val adbUtils: AdbUtils) : AndroidDevice {

  override fun installApkOnDevice(
      apk: File,
      installViaSd: Boolean,
      quiet: Boolean,
      verifyTempWritable: Boolean,
      stagedInstallMode: Boolean,
      userId: String?,
  ): Boolean {
    val elapsed = measureTimeMillis {
      if (verifyTempWritable) {
        try {
          val uniqueFileName = "buck-experiment-${UUID.randomUUID()}"
          executeAdbShellCommand("echo exo > /data/local/tmp/$uniqueFileName")
          executeAdbShellCommand("rm /data/local/tmp/$uniqueFileName")
        } catch (e: AdbCommandFailedException) {
          // TODO: we should check for specific failure here
          LOG.error("Failed to write to /data/local/tmp: ${e.message}")
          throw AndroidInstallException.tempFolderNotWritable()
        }
      }

      val installArgs = buildString {
        append("-r -d")
        // --fastdeploy has a bug, it hides INSTALL_FAILED_UPDATE_INCOMPATIBLE error when there is a
        // mismatch between the apk on the device and the one being installed. The operation will
        // appear as successful without the apk being updated.
        // https://issuetracker.google.com/231040652
        // if (shouldUseFastDeploy()) append(" --fastdeploy")

        if (stagedInstallMode) append(" --staged")
        if (userId != null) append(" --user $userId")
      }

      executeAdbCommandCatching(
          "install $installArgs ${apk.absolutePath}",
          "Failed to install ${apk.name}.",
      )
    }
    val userSuffix = if (userId != null) " for user $userId" else ""
    val kbps = (apk.length() / 1024.0) / (elapsed / 1000.0)
    LOG.info(
        "Installed ${apk.name}$userSuffix (${apk.length()} bytes) in ${elapsed/1000.0} s ($kbps kB/s)"
    )
    return true
  }

  private fun shouldUseFastDeploy(): Boolean {
    val sdkVersion =
        try {
          getProperty("ro.build.version.sdk").toInt()
        } catch (e: Exception) {
          LOG.warn("Unable to determine SDK version, defaulting to legacy install: ${e.message}")
          -1
        }

    return sdkVersion >= MIN_SDK_VERSION_FOR_FASTDEPLOY
  }

  override fun prepareForApexInstallation(): Boolean {
    executeAdbCommand("root")
    sleep(5000)
    executeAdbCommand("wait-for-device")

    // Root kills adbd, and sometimes, it takes a while for it to come back
    for (i in 1..3) {
      if (executeAdbShellCommand("whoami").equals("root")) {
        break
      }
      sleep(1000)
    }

    val softRebootAvailable =
        executeAdbShellCommand("pm", ignoreFailure = true).contains("force-non-staged")
    LOG.info("Soft reboot available: $softRebootAvailable")
    return softRebootAvailable
  }

  override fun installApexOnDevice(
      apex: File,
      quiet: Boolean,
      restart: Boolean,
      softRebootAvailable: Boolean,
  ): Boolean {
    val elapsed = measureTimeMillis {
      try {
        val installArgs = "--apex ${if (softRebootAvailable) "--force-non-staged" else ""}".trim()
        executeAdbCommand("install $installArgs ${apex.absolutePath}")
      } catch (e: AdbCommandFailedException) {
        if ((e.message ?: "").contains("INSTALL_FAILED_VERIFICATION_FAILURE: Staged session ")) {
          throw AndroidInstallException.rebootRequired(
              "Device is already staged; You need to run 'adb reboot' on your device."
          )
        }

        // if the device can't install because the list of native libs is different,
        // retry without the --force-non-staged flag. Then reboot automatically.
        if (
            (e.message ?: "").contains(
                "INSTALL_FAILED_INTERNAL_ERROR: APEX installation failed: Set of native libs required"
            )
        ) {
          // try install again without --force-non-staged
          executeAdbCommandCatching(
              "install -d --apex ${apex.absolutePath}",
              "Failed to install ${apex.name}.",
          )
          throw AndroidInstallException.rebootRequired(
              "Installed ${apex.name} on device; however --force-non-staged doesn't work when the" +
                  " native lib dependencies of an apex have changed. You need to run 'adb" +
                  " reboot' on your device to complete the install. See also:" +
                  " https://www.internalfb.com/intern/wiki/RL/RL_Release_and_Reliability/Build_and_Release_Infra/APEX_in_fbsource/Pit_falls/"
          )
        }

        // If the apex package has changed (e.g. not previously), fall back to
        // remount + push so the apex lands directly in /system/apex.
        if ((e.message ?: "").contains("INSTALL_FAILED_PACKAGE_CHANGED")) {
          LOG.info(
              "INSTALL_FAILED_PACKAGE_CHANGED for ${apex.name}, " +
                  "attempting fallback install via remount and push"
          )
          try {
            // Remount so that we can write to /system_ext/apex
            executeAdbCommand("root")
            executeAdbCommand("wait-for-device")
            executeAdbCommand("remount")

            // Remount sometimes requires a reboot because verity is enabled.
            if (executeAdbShellCommand("getprop ro.boot.veritymode") == "enforcing") {
              executeAdbCommand("reboot")
              executeAdbCommand("wait-for-device")
              waitForBootComplete()

              executeAdbCommand("root")
              executeAdbCommand("wait-for-device")
              executeAdbCommand("remount")
            }

            // Potentially make this customizable in the future.
            executeAdbCommand("push ${apex.absolutePath} /system_ext/apex/")

            // Reboot to activate the apex
            executeAdbCommand("reboot")
            executeAdbCommand("wait-for-device")
            waitForBootComplete()
          } catch (fallbackError: AdbCommandFailedException) {
            throw AndroidInstallException.adbCommandFailedException(
                "Failed to install ${apex.name} via fallback remount+push.",
                fallbackError.message,
            )
          }
          return@measureTimeMillis
        }

        throw AndroidInstallException.adbCommandFailedException(
            "Failed to install ${apex.name}.",
            e.message,
        )
      }

      if (!softRebootAvailable) {
        throw AndroidInstallException.rebootRequired(
            "--force-non-staged is not available on device" +
                "(is the device running an older build?); " +
                "${apex.name} was installed successfully but will not be active until " +
                "you run 'adb reboot' on your device"
        )
      }

      if (restart) {
        try {
          executeAdbShellCommand("stop")
          executeAdbShellCommand("start")

          // Wait for device to be fully ready after soft reboot
          waitForBootComplete()
          waitForPackageManagerReady()
          waitForStorageReady()
        } catch (e: AdbCommandFailedException) {
          throw AndroidInstallException.rebootRequired(
              "Failed to stop+start shell; ${apex.name} was installed successfully but device will be in an unknown state until you run 'adb reboot'"
          )
        }
      }
    }
    val kbps = (apex.length() / 1024.0) / (elapsed / 1000.0)
    LOG.info("Installed ${apex.name} (${apex.length()} bytes) in ${elapsed/1000.0} s ($kbps kB/s)")
    return true
  }

  private fun waitForBootComplete() {
    waitForCondition(
        command = "getprop sys.boot_completed",
        condition = { output -> output.trim() == "1" },
        successMessage = "Boot completed after soft reboot",
        timeoutMessage = "Device did not complete boot after soft reboot within timeout",
        timeoutMs = 80000, // Account for Horizon OS Emulator.
    )
  }

  private fun waitForPackageManagerReady() {
    waitForCondition(
        command = "pm",
        condition = { output -> output.isNotEmpty() && !output.contains("Can't find service") },
        successMessage = "Package manager service ready",
        timeoutMessage = "Package manager service did not become ready within timeout",
    )
  }

  private fun waitForStorageReady() {
    waitForCondition(
        command = "ls /storage/emulated/0 2>&1 || echo STORAGE_NOT_READY",
        condition = { output ->
          !output.contains("Transport endpoint is not connected") &&
              !output.contains("STORAGE_NOT_READY") &&
              !output.contains("No such file or directory")
        },
        successMessage = "Storage filesystem ready",
        timeoutMessage = "Storage filesystem did not become ready within timeout",
        timeoutMs = 30000,
    )
  }

  /**
   * Polls a shell command until a condition is met or timeout is reached.
   *
   * @param command the shell command to execute
   * @param condition a predicate that returns true when the desired state is reached
   * @param successMessage message to log on success
   * @param timeoutMessage message for the exception if timeout is reached
   * @param timeoutMs maximum time to wait in milliseconds
   * @param pollIntervalMs time between polling attempts in milliseconds
   * @throws AndroidInstallException if the condition is not met within the timeout
   */
  private fun waitForCondition(
      command: String,
      condition: (String) -> Boolean,
      successMessage: String,
      timeoutMessage: String,
      timeoutMs: Long = 10000,
      pollIntervalMs: Long = 100,
  ) {
    LOG.info("Waiting for condition (timeout: ${timeoutMs}ms): $successMessage")
    val startTime = System.currentTimeMillis()
    var attempt = 0

    while (System.currentTimeMillis() - startTime < timeoutMs) {
      attempt++
      if (condition(executeAdbShellCommand(command, ignoreFailure = true))) {
        LOG.info(successMessage)
        return
      }
      LOG.info("Attempt $attempt: condition not met, retrying in ${pollIntervalMs}ms...")
      sleep(pollIntervalMs)
    }

    throw AndroidInstallException.rebootRequired(timeoutMessage)
  }

  override fun stopPackage(packageName: String) {
    executeAdbShellCommandCatching(
        "am force-stop $packageName",
        "Failed to stop package $packageName.",
    )
  }

  @Throws(Exception::class)
  override fun getPackageInfo(packageName: String): Optional<PackageInfo> {
    try {
      val output: String = executeAdbShellCommand("pm path $packageName")
      return Optional.of(PackageInfo(output.removePrefix("package:"), "", ""))
    } catch (e: AdbCommandFailedException) {
      LOG.warn("Failed to get package info for $packageName: ${e.message}")
      return Optional.empty()
    }
  }

  @Throws(Exception::class)
  override fun uninstallPackage(packageName: String) {
    executeAdbCommandCatching("uninstall $packageName", "Failed to uninstall $packageName.")
  }

  @Throws(Exception::class)
  override fun getSignature(packagePath: String): String {
    val entry: String =
        executeAdbShellCommand("unzip -l $packagePath | grep -E -o 'META-INF/[A-Z]+\\.SF'").trim()
    val result: String =
        executeAdbShellCommand(
            "unzip -p $packagePath $entry | grep -E 'SHA1-Digest-Manifest:|SHA-256-Digest-Manifest:'"
        )
    val (_, digest) = result.split(":", limit = 2)
    return digest.trim()
  }

  @Throws(Exception::class)
  override fun listDirRecursive(root: Path): ImmutableSortedSet<Path> {
    val lsOutput: String =
        executeAdbCommandCatching("shell ls -R $root", "Failed to list path $root.")

    val paths: MutableSet<Path> = HashSet()
    val dirs: MutableSet<Path?> = HashSet()
    var currentDir: Path? = null
    for (line in Splitter.on(LINE_ENDING).omitEmptyStrings().split(lsOutput)) {
      if (line.endsWith(":")) {
        currentDir = root.relativize(Paths.get(line.removeSuffix(":")))
        dirs.add(currentDir)
      } else {
        checkNotNull(currentDir)
        paths.add(currentDir.resolve(line))
      }
    }
    return ImmutableSortedSet.copyOf<Path>(Sets.difference(paths, dirs))
  }

  override fun rmFiles(dirPath: String, filesToDelete: Iterable<String>) {
    val elapsed: Long = measureTimeMillis {
      val tempFile = File.createTempFile("files_to_delete", ".txt")
      try {
        tempFile.writeText(
            filesToDelete.joinToString("\n") { Paths.get(dirPath).resolve(it).toString() }
        )
        executeAdbCommand("push -z brotli ${tempFile.absolutePath} /data/local/tmp")
        executeAdbShellCommand("cat /data/local/tmp/${tempFile.name} | xargs rm -f")
      } catch (e: AdbCommandFailedException) {
        throw AndroidInstallException.adbCommandFailedException(
            "Failed delete ${filesToDelete.count()} files from $dirPath.",
            e.message,
        )
      } finally {
        tempFile.delete()
        executeAdbShellCommand("rm -f /data/local/tmp/${tempFile.name}")
      }
    }
    LOG.info("Deleted ${filesToDelete.count()} files from $dirPath in ${elapsed/1000.0} seconds.")
  }

  @Throws(Exception::class)
  override fun createForward(): AutoCloseable {
    return AutoCloseable {}
  }

  @Throws(Exception::class)
  override fun installFiles(filesType: String, installPaths: Map<Path, Path>) {
    LOG.debug(
        "%s: %s",
        filesType,
        installPaths
            .map { "${it.value.parent} -> ${it.key.parent}" }
            .distinct()
            .joinToString(separator = "\n\t", prefix = "[", postfix = "]"),
    )
    val timeSpent: Long = measureTimeMillis {
      when (filesType) {
        // 1- create a temp folder for each destination folder
        // 2- copy all the files to the temp folder. adb does not support symbolic links so
        // this is necessary
        // 3- push the temp folder to the device using one call to adb. this is much faster
        // than pushing each file individually
        "secondary_dex",
        "native_library",
        "resources" -> {
          // create a temp folder for each destination folder
          val tempFolders = mutableMapOf<Path, Path>()
          installPaths.keys
              .stream()
              .map { it.parent }
              .distinct()
              .forEach { tempFolders[it] = Files.createTempDirectory("${it.fileName}_") }
          installPaths.forEach { (destination, source) ->
            val targetPath = tempFolders[destination.parent]?.resolve(destination.fileName)
            Files.copy(source, targetPath, StandardCopyOption.REPLACE_EXISTING)
          }
          // TODO consider tarballing all the files in the temp for faster transfer
          // push the temp folder to the device
          tempFolders.forEach { (destination, source) ->
            try {
              executeAdbCommand("push -z brotli $source /data/local/tmp")
              executeAdbShellCommand("mv /data/local/tmp/${source.fileName}/* $destination")
              // instagram will fail to star if dex files are writable
              executeAdbShellCommand("chmod 644 $destination/*")
              executeAdbShellCommand("rm -rf /data/local/tmp/${source.fileName}")
            } catch (e: AdbCommandFailedException) {
              throw AndroidInstallException.adbCommandFailedException(
                  "Failed to push $source to $destination.",
                  e.message,
              )
            }
          }
          // delete temp folder
          tempFolders.keys.map { it.toFile() }.forEach { it.deleteRecursively() }
        }
        else -> {
          installPaths.forEach { (destination, source) ->
            LOG.debug("\tPushing $source to $destination")
            executeAdbCommandCatching(
                "push $source $destination",
                "Failed to push $source to $destination.",
            )
          }
        }
      }
    }
    LOG.info("$filesType: Transferred ${installPaths.size} files in ${timeSpent/1000.0} seconds")
  }

  @Throws(Exception::class)
  override fun mkDirP(dirpath: String) {
    executeAdbShellCommandCatching(
        "umask 022 && mkdir -p $dirpath",
        "Failed to create dir $dirpath.",
    )
  }

  @Throws(Exception::class)
  override fun getProperty(name: String): String {
    return executeAdbShellCommandCatching("getprop $name", "Failed to get property $name.")
  }

  @Throws(Exception::class)
  override fun getDeviceAbis(): List<String> {
    val abiListProperty = getProperty("ro.product.cpu.abilist")
    if (abiListProperty.isNotEmpty()) {
      return abiListProperty.split(',')
    } else {
      return listOf(getProperty("ro.product.cpu.abi"), getProperty("ro.product.cpu.abi2"))
          .filter { it.isEmpty() }
          .takeIf { it.isNotEmpty() } ?: throw AndroidInstallException.deviceAbiUnknown()
    }
  }

  @Throws(Exception::class)
  override fun killProcess(processName: String) {
    throw AndroidInstallException.operationNotSupported("killProcess")
  }

  override fun getSerialNumber(): String {
    return serial
  }

  @Throws(Exception::class)
  override fun getWindowManagerProperty(propertyName: String): String {
    throw AndroidInstallException.operationNotSupported("getWindowManagerProperty")
  }

  override fun uninstallApkFromDevice(packageName: String, keepData: Boolean): Boolean {
    try {
      executeAdbShellCommand("rm -rf ${ExopackageInstaller.EXOPACKAGE_INSTALL_ROOT}/$packageName")
      executeAdbShellCommand("pm uninstall ${if (keepData) "-k " else ""} $packageName")
      return true
    } catch (e: AdbCommandFailedException) {
      LOG.warn("Failed to uninstall $packageName: ${e.message}")
      return false
    }
  }

  /**
   * Sometimes installation fails and leaves the root directory in a weird state. Adding a cheap
   * fix-root at the beginning to make sure all the folders have access permission.
   */
  override fun fixRootDir(rootDir: String) {
    LOG.info("Fixing root dir $rootDir")
    executeAdbShellCommandCatching(
        "find $rootDir -type d -exec chmod a+x {} +",
        "Failed to fix root dir $rootDir.",
    )
  }

  override fun setDebugAppPackageName(packageName: String?): Boolean {
    if (packageName != null) {
      executeAdbShellCommand(adbUtils.getAmSetDebugAppCommand(packageName))
    }
    return true
  }

  override fun enableAppLinks(packageName: String?) {
    if (packageName != null) {
      executeAdbShellCommand("pm set-app-links --package $packageName 1 all")
    }
  }

  override fun getInstallerMethodName(): String = "adb_installer"

  override fun getDiskSpace(): List<String> {
    try {
      val result: String = executeAdbShellCommand("df -h /data | awk '{print \$2, \$3, \$4}'")
      val (size, used, available) = result.lines()[1].split(" ", limit = 3)
      return listOf(size, used, available)
    } catch (e: Exception) {
      LOG.warn("Failed to get disk space: $e")
      return listOf("_", "_", "_")
    }
  }

  override fun isEmulator(): Boolean {
    return isLocalTransport() || getProperty("ro.kernel.qemu") == "1"
  }

  override fun isOnline(): Boolean {
    return getState() == "device"
  }

  private fun getState(): String {
    return try {
      adbUtils.executeAdbCommand("get-state", serialNumber)
    } catch (e: AdbCommandFailedException) {
      // When a device is offline, adb get-state fails with exit code 1.
      // Return "offline" to indicate the device state instead of throwing an exception.
      // This allows the installer to continue with other available devices.
      LOG.warn("Failed to get state for device $serialNumber: ${e.message}")
      "offline"
    }
  }

  /**
   * To be consistent with adb, we treat all local transports (as opposed to USB transports) as
   * emulators instead of devices.
   */
  private fun isLocalTransport(): Boolean {
    /** Pattern that matches Genymotion serial numbers; ex. 127.0.0.1:15562 */
    return Pattern.compile("\\d+\\.\\d+\\.\\d+\\.\\d+:\\d+").matcher(serialNumber).find()
  }

  @Throws(Exception::class)
  override fun installBuildUuidFile(
      dataRoot: Path,
      packageName: String,
      buildUuid: String,
  ): Boolean {
    val destinationPath: String = dataRoot.resolve(packageName).toString()
    try {
      executeAdbShellCommand("umask 022 && mkdir -p $destinationPath")
      executeAdbShellCommand("echo $buildUuid > $destinationPath/build_uuid.txt")
    } catch (e: Exception) {
      // we don't want to fail the install if we can't install the build_uuid.txt file
      LOG.warn("Failed to install build_uuid.txt file on $serial: ${e.message}")
    }
    return true
  }

  override fun deviceStartIntent(intent: AndroidIntent?): String {
    if (intent == null) {
      return ""
    }

    try {
      executeAdbShellCommand(AndroidIntent.getAmStartCommand(intent))
      return ""
    } catch (e: AdbCommandFailedException) {
      throw AndroidInstallException.adbCommandFailedException("Failed to start intent.", e.message)
    }
  }

  private fun executeAdbShellCommandCatching(command: String, message: String): String {
    try {
      return adbUtils.executeAdbShellCommand(command, serialNumber)
    } catch (e: AdbCommandFailedException) {
      throw AndroidInstallException.adbCommandFailedException(message, e.message)
    }
  }

  private fun executeAdbCommandCatching(command: String, message: String): String {
    try {
      return adbUtils.executeAdbCommand(command, serialNumber)
    } catch (e: AdbCommandFailedException) {
      throw AndroidInstallException.adbCommandFailedException(message, e.message)
    }
  }

  private fun executeAdbShellCommand(command: String, ignoreFailure: Boolean = false): String =
      adbUtils.executeAdbShellCommand(command, serialNumber, ignoreFailure)

  private fun executeAdbCommand(command: String, ignoreFailure: Boolean = false): String =
      adbUtils.executeAdbCommand(command, serialNumber, ignoreFailure)

  companion object {
    private val LINE_ENDING: Pattern = Pattern.compile("\r?\n")
    private val LOG: Logger = Logger.get(AndroidDeviceImpl::class.java.name)

    // --fastdeploy is only supported on Android 10+ (API 29+)
    // https://developer.android.com/tools/releases/platform-tools#2905_october_2019
    private const val MIN_SDK_VERSION_FOR_FASTDEPLOY = 29
  }
}
