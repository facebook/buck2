/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.cd.workertool;

import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.jvm.cd.CompilerDaemonLoggerUtil;
import com.facebook.buck.jvm.cd.CompilerDaemonRunner;
import com.google.common.collect.ImmutableMap;
import java.io.IOException;
import java.util.logging.Level;
import org.kohsuke.args4j.CmdLineException;

/**
 * KotlinCD main class.
 *
 * <p>This provides a simple executable that can run any of the kotlincd actions.
 */
public class KotlinCDMain {
  private static final String LOG_PATH = "buck-out/v2/kotlincd";

  /** Main entrypoint of KotlinCD worker tool. */
  public static void main(String[] args) throws IOException {
    try {
      CompilerDaemonLoggerUtil.setDefaultLogger("kotlincd_worker", LOG_PATH);
      CompilerDaemonLoggerUtil.setConsoleHandlerLogLevelTo(Level.WARNING);
      KotlinCDCommand command = new KotlinCDCommand(args, ImmutableMap.copyOf(System.getenv()));
      Logger logger = Logger.get(KotlinCDMain.class.getName());
      logger.info(String.format("Starting KotlinCDWorkerTool %s", command));
      CompilerDaemonRunner.run(command);
      command.postExecute();
    } catch (CmdLineException e) {
      System.exit(1);
    }
    System.exit(0);
  }
}
