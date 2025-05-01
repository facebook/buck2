/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.exopackage;

import com.android.ddmlib.InstallException;
import com.facebook.buck.android.agent.util.AgentUtil;
import com.facebook.buck.core.util.log.Logger;
import com.google.common.base.Throwables;
import java.io.File;
import java.nio.file.Path;
import java.util.Optional;
import javax.annotation.Nullable;

class ExopackageAgent {

  private static final Logger LOG = Logger.get(ExopackageInstaller.class);

  private final String classPath;
  private final String nativeAgentPath;

  public ExopackageAgent(String classPath, String nativeAgentPath) {
    this.classPath = classPath;
    this.nativeAgentPath = nativeAgentPath;
  }

  String getAgentCommand() {
    return getAgentCommand(/* javaLibraryPath */ null);
  }

  /**
   * @param javaLibraryPath The java library path that we want to use for the dalvikvm call. This is
   *     required because dalvikvm doesn't set up the java library path to point to the relevant
   *     directories within the APK.
   */
  String getAgentCommand(@Nullable String javaLibraryPath) {
    if (javaLibraryPath != null) {
      return "dalvikvm -Djava.library.path="
          + javaLibraryPath
          + " -classpath "
          + classPath
          + " com.facebook.buck.android.agent.AgentMain ";
    } else {
      return "dalvikvm -classpath " + classPath + " com.facebook.buck.android.agent.AgentMain ";
    }
  }

  String getNativePath() {
    return nativeAgentPath;
  }

  public String getMkDirCommand() {
    // Kind of a hack here.  The java agent can't force the proper permissions on the
    // directories it creates, so we use the command-line "mkdir -p" instead of the java agent.
    // Fortunately, "mkdir -p" seems to work on all devices where we use use the java agent.
    return "mkdir -p";
  }

  public static ExopackageAgent installAgentIfNecessary(AndroidDevice device, Path agentApkPath) {
    try {
      Optional<PackageInfo> agentInfo = device.getPackageInfo(AgentUtil.AGENT_PACKAGE_NAME);
      if (agentInfo.isPresent()
          && !agentInfo.get().versionCode.equals(AgentUtil.AGENT_VERSION_CODE)) {
        LOG.debug(
            "Agent version mismatch. Wanted %s, got %s.",
            AgentUtil.AGENT_VERSION_CODE, agentInfo.get().versionCode);
        // Always uninstall before installing.  We might be downgrading, which requires
        // an uninstall, or we might just want a clean installation.
        uninstallAgent(device);
        agentInfo = Optional.empty();
      }
      if (agentInfo.isEmpty()) {
        LOG.debug("Installing agent.");
        installAgentApk(device, agentApkPath);
        agentInfo = device.getPackageInfo(AgentUtil.AGENT_PACKAGE_NAME);
      }
      PackageInfo packageInfo = agentInfo.get();
      return new ExopackageAgent(packageInfo.apkPath, packageInfo.nativeLibPath);
    } catch (Exception e) {
      Throwables.throwIfUnchecked(e);
      throw new RuntimeException(e);
    }
  }

  private static void uninstallAgent(AndroidDevice device) throws InstallException {
    try {
      device.uninstallPackage(AgentUtil.AGENT_PACKAGE_NAME);
    } catch (Exception e) {
      throw new InstallException(e);
    }
  }

  private static void installAgentApk(AndroidDevice device, Path agentApkPath) {
    try {
      File apkPath = agentApkPath.toFile();
      boolean success =
          device.installApkOnDevice(
              apkPath, /* installViaSd */ false, /* quiet */ false, /* stagedInstallMode */ false);
      if (!success) {
        throw new RuntimeException();
      }
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
