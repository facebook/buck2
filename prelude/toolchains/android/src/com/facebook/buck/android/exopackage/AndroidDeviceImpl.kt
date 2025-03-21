/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.exopackage

import com.android.ddmlib.InstallException
import com.facebook.buck.core.util.log.Logger
import com.facebook.buck.installer.android.AndroidInstallException
import com.google.common.base.Splitter
import com.google.common.collect.ImmutableSortedSet
import com.google.common.collect.Sets
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardCopyOption
import java.util.Optional
import java.util.regex.Pattern
import kotlin.system.measureTimeMillis

class AndroidDeviceImpl(val serial: String) : AndroidDevice {

  override fun installApkOnDevice(
      apk: File,
      installViaSd: Boolean,
      quiet: Boolean,
      verifyTempWritable: Boolean,
      stagedInstallMode: Boolean
  ): Boolean {
    // TODO: this can be checked sooner when we receive install options
    if (stagedInstallMode) {
      throw AndroidInstallException.operationNotSupported("stagedInstallMode")
    }

    val elapsed = measureTimeMillis {
      if (verifyTempWritable) {
        try {
          executeAdbShellCommand("echo exo > /data/local/tmp/buck-experiment")
          executeAdbShellCommand("rm /data/local/tmp/buck-experiment")
        } catch (e: AdbCommandFailedException) {
          // TODO: we should check for specific failure here
          throw AndroidInstallException.tempFolderNotWritable()
        }
      }
      // TODO consider using --fastdeploy after intaller is stable
      executeAdbCommandCatching(
          "install -r -d ${apk.absolutePath}", "Failed to install ${apk.name}.")
    }
    val kbps = (apk.length() / 1024.0) / (elapsed / 1000.0)
    LOG.info("Installed ${apk.name} (${apk.length()} bytes) in ${elapsed/1000.0} s ($kbps kB/s)")
    return true
  }

  override fun installApexOnDevice(apex: File, quiet: Boolean): Boolean {
    throw AndroidInstallException.operationNotSupported("installApexOnDevice")
  }

  override fun stopPackage(packageName: String) {
    executeAdbShellCommandCatching(
        "am force-stop $packageName", "Failed to stop package $packageName.")
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

  @Throws(InstallException::class)
  override fun uninstallPackage(packageName: String) {
    executeAdbCommandCatching("uninstall $packageName", "Failed to uninstall $packageName.")
  }

  @Throws(Exception::class)
  override fun getSignature(packagePath: String): String {
    val entry: String =
        executeAdbShellCommand("unzip -l $packagePath | grep -E -o 'META-INF/[A-Z]+\\.SF'").trim()
    val result: String =
        executeAdbShellCommand(
            "unzip -p $packagePath $entry | grep -E 'SHA1-Digest-Manifest:|SHA-256-Digest-Manifest:'")
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
      for (file in filesToDelete) {
        executeAdbShellCommandCatching("rm $dirPath/$file", "Failed to delete $dirPath/$file.")
      }
    }
    LOG.info("Deleted ${filesToDelete.count()} files in $dirPath in ${elapsed/1000.0} seconds.")
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
            .joinToString(separator = "\n\t", prefix = "[", postfix = "]"))
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
                  "Failed to push $source to $destination.", e.message)
            }
          }
          // delete temp folder
          tempFolders.keys.map { it.toFile() }.forEach { it.deleteRecursively() }
        }
        else -> {
          installPaths.forEach { (destination, source) ->
            LOG.debug("\tPushing $source to $destination")
            executeAdbCommandCatching(
                "push $source $destination", "Failed to push $source to $destination.")
          }
        }
      }
    }
    LOG.info("$filesType: Transferred ${installPaths.size} files in ${timeSpent/1000.0} seconds")
  }

  @Throws(Exception::class)
  override fun mkDirP(dirpath: String) {
    executeAdbShellCommandCatching(
        "umask 022 && mkdir -p $dirpath", "Failed to create dir $dirpath.")
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
      throw AndroidInstallException.adbCommandFailedException(
          "Failed to uninstall $packageName.", e.message)
    }
  }

  /**
   * Sometimes installation fails and leaves the root directory in a weird state. Adding a cheap
   * fix-root at the beginning to make sure all the folders have access permission.
   */
  override fun fixRootDir(rootDir: String) {
    LOG.info("Fixing root dir $rootDir")
    executeAdbShellCommandCatching(
        "find $rootDir -type d -exec chmod a+x {} +", "Failed to fix root dir $rootDir.")
  }

  override fun getInstallerMethodName(): String = "adb_installer"

  override fun isEmulator(): Boolean {
    return getProperty("ro.kernel.qemu").equals("1")
  }

  @Throws(Exception::class)
  override fun installBuildUuidFile(
      dataRoot: Path,
      packageName: String,
      buildUuid: String
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
    try {
      // Use set-debug-app to silence ANRs while running.
      AndroidIntent.getAmSetDebugAppCommand(intent)?.let { executeAdbShellCommand(it) }
      AndroidIntent.getAmStartCommand(intent)?.let { executeAdbShellCommand(it) }
      return ""
    } catch (e: AdbCommandFailedException) {
      throw AndroidInstallException.adbCommandFailedException("Failed to start intent.", e.message)
    }
  }

  private fun executeAdbShellCommandCatching(command: String, message: String): String {
    try {
      return AdbUtils.executeAdbShellCommand(command, serialNumber)
    } catch (e: AdbCommandFailedException) {
      throw AndroidInstallException.adbCommandFailedException(message, e.message)
    }
  }

  private fun executeAdbCommandCatching(command: String, message: String): String {
    try {
      return AdbUtils.executeAdbCommand(command, serialNumber)
    } catch (e: AdbCommandFailedException) {
      throw AndroidInstallException.adbCommandFailedException(message, e.message)
    }
  }

  private fun executeAdbShellCommand(command: String): String =
      AdbUtils.executeAdbShellCommand(command, serialNumber)

  private fun executeAdbCommand(command: String): String =
      AdbUtils.executeAdbCommand(command, serialNumber)

  companion object {
    private val LINE_ENDING: Pattern = Pattern.compile("\r?\n")
    private val LOG: Logger = Logger.get(AndroidDeviceImpl::class.java.name)
  }
}
