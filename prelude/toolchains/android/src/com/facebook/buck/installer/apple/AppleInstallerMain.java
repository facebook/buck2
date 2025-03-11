/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.installer.apple;

import com.facebook.buck.installer.InstallerServer;
import java.io.IOException;
import java.util.logging.FileHandler;
import java.util.logging.Level;
import java.util.logging.Logger; // NOPMD
import java.util.logging.SimpleFormatter;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;

/**
 * Main entry point for executing {@code Installs for buck2 apple}.
 *
 * <p>Expected usage: {@code this_binary options}.
 */
public class AppleInstallerMain {

  static {
    // set java.util.logging (JUL) simple formatter to 1 liner.
    System.setProperty(
        "java.util.logging.SimpleFormatter.format",
        "[%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS] [%4$s] %5$s%6$s%n");
  }

  /** Main Entry Point */
  public static void main(String[] args) throws IOException, InterruptedException {
    AppleInstallerMain installer = new AppleInstallerMain();
    AppleCommandLineOptions options = new AppleCommandLineOptions();
    CmdLineParser parser = new CmdLineParser(options);
    try {
      parser.parseArgument(args);
      installer.run(options, getLogger(options.getLogPath()));
      System.exit(0);
    } catch (CmdLineException e) {
      System.err.println(e.getMessage());
      parser.printUsage(System.err);
      System.exit(1);
    }
  }

  private static Logger getLogger(String logPath) throws IOException {
    Logger logger = Logger.getLogger(AppleInstallerMain.class.getName());
    logger.addHandler(getFileHandler(logPath));
    return logger;
  }

  private static FileHandler getFileHandler(String logPath) throws IOException {
    FileHandler fileHandler = new FileHandler(logPath);
    fileHandler.setFormatter(new SimpleFormatter());
    fileHandler.setLevel(Level.INFO);
    return fileHandler;
  }

  private void run(AppleCommandLineOptions options, Logger logger)
      throws IOException, InterruptedException {
    AppleInstallerManager am = new AppleInstallerManager(logger, options);
    /** Starts the GRPC Server */
    InstallerServer server = new InstallerServer(am, logger, options.getTcpPort());
    server.run();
  }
}
