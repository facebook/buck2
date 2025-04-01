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
      Logger.getLogger("").addHandler(getFileHandler(options.getLogPath()));
      installer.run(options);
      System.exit(0);
    } catch (CmdLineException e) {
      System.err.println(e.getMessage());
      parser.printUsage(System.err);
      System.exit(1);
    }
  }

  private static FileHandler getFileHandler(String logPath) throws IOException {
    FileHandler fileHandler = new FileHandler(logPath);
    fileHandler.setFormatter(new SimpleFormatter());
    fileHandler.setLevel(Level.INFO);
    return fileHandler;
  }

  private void run(AppleCommandLineOptions options) throws IOException, InterruptedException {
    AppleInstallerManager am = new AppleInstallerManager(options);
    /** Starts the GRPC Server */
    InstallerServer server = new InstallerServer(am, options.getTcpPort());
    server.run();
  }
}
