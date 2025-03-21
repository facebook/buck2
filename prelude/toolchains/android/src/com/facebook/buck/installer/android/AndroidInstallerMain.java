/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.installer.android;

import com.facebook.buck.installer.InstallerServer;
import java.io.IOException;
import java.util.logging.FileHandler;
import java.util.logging.Level;
import java.util.logging.Logger; // NOPMD
import java.util.logging.SimpleFormatter;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;

/**
 * Main entry point for executing {@code Installs for buck2 android}.
 *
 * <p>Expected usage: {@code this_binary options}.
 */
public class AndroidInstallerMain {
  static {
    // set java.util.logging (JUL) simple formatter to 1 liner.
    System.setProperty(
        "java.util.logging.SimpleFormatter.format",
        "[%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS] [%4$s] %3$s - %5$s%6$s%n");
  }

  /** Main Entry Point */
  public static void main(String[] args) throws IOException, InterruptedException {
    AndroidInstallerMain installer = new AndroidInstallerMain();
    AndroidCommandLineOptions options = new AndroidCommandLineOptions();
    CmdLineParser parser = new CmdLineParser(options);
    try {
      parser.parseArgument(args);
      Logger.getLogger("").addHandler(getFileHandler(options.getLogPath()));
      Logger logger = getLogger();
      installer.run(options, logger);
      logger.info("server shutdown, exiting");
      System.exit(0);
    } catch (CmdLineException e) {
      System.out.println(e.getMessage());
      parser.printUsage(System.out);
      System.exit(1);
    }
  }

  private static Logger getLogger() throws IOException {
    return Logger.getLogger(AndroidInstallerMain.class.getName());
  }

  private static FileHandler getFileHandler(String logPath) throws IOException {
    FileHandler fileHandler = new FileHandler(logPath);
    fileHandler.setFormatter(new SimpleFormatter());
    fileHandler.setLevel(Level.INFO);
    return fileHandler;
  }

  private void run(AndroidCommandLineOptions options, Logger logger)
      throws IOException, InterruptedException {
    AndroidInstallerManager androidInstallerManager = new AndroidInstallerManager(logger, options);
    /** Starts the GRPC Server */
    InstallerServer server =
        new InstallerServer(androidInstallerManager, logger, options.getTcpPort());
    server.run();
  }
}
