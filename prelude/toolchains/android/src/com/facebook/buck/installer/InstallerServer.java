/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.installer;

import static com.google.common.util.concurrent.MoreExecutors.directExecutor;

import com.facebook.buck.util.types.Unit;
import com.google.common.util.concurrent.FutureCallback;
import com.google.common.util.concurrent.Futures;
import com.google.common.util.concurrent.SettableFuture;
import io.grpc.Server;
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder;
import io.grpc.netty.shaded.io.netty.channel.EventLoopGroup;
import io.grpc.netty.shaded.io.netty.channel.nio.NioEventLoopGroup;
import io.grpc.netty.shaded.io.netty.channel.socket.nio.NioServerSocketChannel;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger; // NOPMD

public class InstallerServer {
  public InstallerServer(InstallCommand installer, Logger logger, int tcpPort)
      throws IOException, InterruptedException {
    SettableFuture<Unit> isDone = SettableFuture.create();
    InstallerService installerService = new InstallerService(installer, isDone, logger);
    Server grpcServer = buildServer(installerService, logger, tcpPort);
    Futures.addCallback(
        isDone,
        new FutureCallback<>() {
          @Override
          public void onSuccess(Unit unit) {
            logger.info("Installer Server shutting down...");
            stopServer(grpcServer);
          }

          @Override
          public void onFailure(Throwable t) {
            logger.log(Level.WARNING, "Execution exception...", t);
          }
        },
        directExecutor());

    Runtime.getRuntime()
        .addShutdownHook(
            new Thread(
                () -> {
                  // Use stderr here since the logger may have been reset by its JVM shutdown hook.
                  System.err.println("*** shutting down gRPC server since JVM is shutting down");
                  stopServer(grpcServer);
                  System.err.println("*** server shut down");
                }));

    grpcServer.start();

    do {
      try {
        if (!isParentAlive()) {
          logger.info("Parent process has been killed, shutting down");
          stopServer(grpcServer);
        }
      } catch (Exception e) {
        logger.log(Level.SEVERE, "Error checking parent process liveness", e);
      }
    } while (!grpcServer.awaitTermination(10, TimeUnit.SECONDS));
  }

  /** Build an installer server at requested {@code TCP}. */
  private Server buildServer(InstallerService service, Logger logger, int tcpPort) {
    logger.info(String.format("Starting Installer Server using TCP on %s", tcpPort));
    return getTCPServer(service, logger, tcpPort);
  }

  private Server getTCPServer(InstallerService service, Logger logger, int tcpPort) {
    EventLoopGroup group = new NioEventLoopGroup();
    SocketAddress addr = new InetSocketAddress("127.0.0.1", tcpPort); // NOPMD
    Server server =
        NettyServerBuilder.forAddress(addr)
            .channelType(NioServerSocketChannel.class)
            .workerEventLoopGroup(group)
            .bossEventLoopGroup(group)
            .addService(service)
            .build();
    logger.info(String.format("Starting server listening on TCP port %s", tcpPort));
    return server;
  }

  /** Shuts down installer server. */
  private void stopServer(Server grpcServer) {
    if (grpcServer != null && !grpcServer.isTerminated()) {
      try {
        grpcServer.shutdown().awaitTermination();
        if (!grpcServer.isTerminated()) {
          grpcServer.shutdownNow();
        }
      } catch (InterruptedException e) {
        grpcServer.shutdownNow();
      }
    }
  }

  private static boolean isParentAlive() {
    Optional<ProcessHandle> parent = ProcessHandle.current().parent();
    return parent.isPresent() && parent.get().pid() != 1 && parent.get().isAlive();
  }
}
