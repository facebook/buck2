/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd.workertool.grpc;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import io.grpc.ServerServiceDefinition;
import java.nio.file.Path;
import java.util.Optional;
import org.junit.Test;

public class WorkerGrpcServerTest {

  private WorkerGrpcServer createServer(String name) {
    WorkerGrpcService mockService = mock(WorkerGrpcService.class);
    when(mockService.bindService()).thenReturn(ServerServiceDefinition.builder("test").build());
    Path socketPath = Path.of("/tmp/test-worker-grpc-server-" + System.nanoTime() + ".sock");
    return new WorkerGrpcServer(name, socketPath, mockService);
  }

  @Test
  public void testEventLoopGroupHasMinimalThreadCount() {
    WorkerGrpcServer server = createServer("TestServer");

    assertEquals(
        "Event loop group should have exactly 1 thread", 1, server.getEventLoopThreadCount());
  }

  @Test
  public void testHealthCheckThreadIsDaemon() throws InterruptedException {
    String serverName = "DaemonTest-" + System.nanoTime();
    createServer(serverName);

    Thread.sleep(200);

    Optional<Thread> healthCheckThread =
        Thread.getAllStackTraces().keySet().stream()
            .filter(t -> t.getName().contains(serverName + "-health-check"))
            .findFirst();

    assertTrue(
        "Health check thread should exist with name containing '"
            + serverName
            + "-health-check'",
        healthCheckThread.isPresent());
    assertTrue("Health check thread should be a daemon thread", healthCheckThread.get().isDaemon());
  }
}
