/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.stepsbuilder.javacd.main;

import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.jvm.cd.CompilerDaemonLoggerUtil;
import com.facebook.buck.jvm.cd.CompilerDaemonRunner;
import com.google.common.collect.ImmutableMap;
import java.io.IOException;
import java.util.logging.Level;
import org.kohsuke.args4j.CmdLineException;

/**
 * JavaCD main class.
 *
 * <p>This provides a simple executable that can run any of the javacd actions.
 */
public class JavaCDMain {
  private static final String LOG_PATH = "buck-out/v2/javacd";

  /** Main entrypoint of JavaCD worker tool. */
  public static void main(String[] args) throws IOException {
    try {
      JavaCDCommand command = new JavaCDCommand(args, ImmutableMap.copyOf(System.getenv()));
      CompilerDaemonLoggerUtil.setDefaultLogger("javacd_worker", LOG_PATH);
      CompilerDaemonLoggerUtil.setConsoleHandlerLogLevelTo(Level.WARNING);
      Logger logger = Logger.get(JavaCDMain.class.getName());
      CompilerDaemonRunner.run(command);
      logger.info(String.format("Starting JavaCDWorkerTool %s", command));
      command.maybeWriteClassAbi();
      command.maybeWriteAbiDir();
      command.maybeWriteDepFile();
      command.maybeWriteUsedJarsFile();
    } catch (CmdLineException e) {
      System.exit(1);
    }
    System.exit(0);
  }
}
