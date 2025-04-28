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
import com.facebook.buck.jvm.cd.workertool.grpc.WorkerGrpcServer;
import java.io.IOException;

/**
 * JavaCD grpc worker tool daemon, for use by buck2
 *
 * <p>This starts a grpc service over a uds socket, that accepts javacd compilation commands
 */
public class JavaCDGrpcWorkerToolMain {
  private static final String LOG_PATH = "buck-out/v2/javacd";

  public static void main(String[] args) throws IOException {
    CompilerDaemonLoggerUtil.setDefaultLogger("javacd_grpc_worker", LOG_PATH);
    Logger logger = Logger.get(JavaCDGrpcWorkerToolMain.class.getName());
    logger.info("Starting JavaCD Worker GRPC Server");
    WorkerGrpcServer.runServer("JavaCD", JavaCDCommand::new);
  }
}
