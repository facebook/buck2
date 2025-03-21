/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.installer.apple;

import com.facebook.buck.apple.simulator.AppleDeviceController;
import com.facebook.buck.installer.InstallCommand;
import com.facebook.buck.installer.InstallId;
import com.facebook.buck.installer.InstallResult;
import com.facebook.buck.util.Ansi;
import com.facebook.buck.util.Console;
import com.facebook.buck.util.DefaultProcessExecutor;
import com.facebook.buck.util.Verbosity;
import com.google.common.base.Throwables;
import com.google.common.util.concurrent.SettableFuture;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger; // NOPMD

/** Apple install manager */
class AppleInstallerManager implements InstallCommand {

  private static final Console CONSOLE =
      new Console(Verbosity.STANDARD_INFORMATION, System.out, System.err, Ansi.withoutTty());

  public final Map<InstallId, SettableFuture<AppleInstallAppOptions>> installIdToOptionsMap =
      new HashMap<>();
  private final Logger logger;
  private final AppleCommandLineOptions options;

  public AppleInstallerManager(Logger logger, AppleCommandLineOptions options) {
    this.logger = logger;
    this.options = options;
  }

  @Override
  public InstallResult fileReady(String artifactName, Path artifactPath, InstallId installId) {
    SettableFuture<AppleInstallAppOptions> appInstallOptionsFuture = getOptionsFuture(installId);
    // if options file
    if (artifactName.equals("options")) {
      try {
        appInstallOptionsFuture.set(new AppleInstallAppOptions(artifactPath));
        return InstallResult.success();
      } catch (Exception err) {
        String errMsg = Throwables.getStackTraceAsString(err);
        logger.log(
            Level.SEVERE,
            String.format(
                "Error creating AppleInstallAppOptions from `install_apple_data`. Error message:"
                    + " %s",
                errMsg),
            err);
        return InstallResult.error(errMsg);
      }
    }

    // process app installation
    try {
      // wait till ready
      AppleInstallAppOptions appleInstallAppOptions = appInstallOptionsFuture.get();

      DefaultProcessExecutor processExecutor = new DefaultProcessExecutor(CONSOLE);
      AppleDeviceController appleDeviceController =
          new AppleDeviceController(processExecutor, Paths.get("/usr/local/bin/idb"));
      AppleInstall appleInstall =
          new AppleInstall(
              processExecutor,
              options,
              appleDeviceController,
              appleInstallAppOptions,
              logger,
              artifactPath);
      return appleInstall.installAppleBundle(artifactPath);
    } catch (Exception err) {
      String errMsg = Throwables.getStackTraceAsString(err);
      return InstallResult.error(errMsg);
    }
  }

  @Override
  public InstallResult allFilesReady(InstallId installId) {
    return InstallResult.success();
  }

  private SettableFuture<AppleInstallAppOptions> getOptionsFuture(InstallId installId) {
    synchronized (installIdToOptionsMap) {
      return installIdToOptionsMap.computeIfAbsent(installId, ignore -> SettableFuture.create());
    }
  }
}
