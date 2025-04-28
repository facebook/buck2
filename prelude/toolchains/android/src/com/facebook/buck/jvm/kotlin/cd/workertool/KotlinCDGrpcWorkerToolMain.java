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
import com.facebook.buck.jvm.cd.workertool.grpc.WorkerGrpcServer;
import java.io.IOException;

/**
 * KotlinCD grpc worker tool daemon, for use by buck2
 *
 * <p>This starts a grpc service over a uds socket, that accepts kotlincd compilation commands
 */
public class KotlinCDGrpcWorkerToolMain {
  private static final String LOG_PATH = "buck-out/v2/kotlincd";

  public static void main(String[] args) throws IOException {
    CompilerDaemonLoggerUtil.setDefaultLogger("kotlincd_grpc_worker", LOG_PATH);
    Logger logger = Logger.get(KotlinCDGrpcWorkerToolMain.class.getName());
    logger.info("Starting KotlinCD Worker GRPC Server");
    WorkerGrpcServer.runServer("KotlinCD", KotlinCDCommand::new);
  }
}
