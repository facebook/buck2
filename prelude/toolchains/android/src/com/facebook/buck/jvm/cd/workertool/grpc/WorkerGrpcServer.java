/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd.workertool.grpc;

import static com.facebook.buck.util.environment.EnvVariablesProvider.getRequiredEnvVar;

import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.jvm.cd.CompilerDaemonRunner;
import com.facebook.buck.util.Ansi;
import com.facebook.buck.util.Console;
import com.facebook.buck.util.Verbosity;
import com.facebook.buck.util.perf.PerfStatsTracking;
import com.facebook.buck.util.unit.SizeUnit;
import io.grpc.ForwardingServerCallListener;
import io.grpc.Metadata;
import io.grpc.Server;
import io.grpc.ServerCall;
import io.grpc.ServerCallHandler;
import io.grpc.ServerInterceptor;
import io.grpc.netty.NettyServerBuilder;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.epoll.Epoll;
import io.netty.channel.epoll.EpollEventLoopGroup;
import io.netty.channel.epoll.EpollServerDomainSocketChannel;
import io.netty.channel.kqueue.KQueue;
import io.netty.channel.kqueue.KQueueEventLoopGroup;
import io.netty.channel.kqueue.KQueueServerDomainSocketChannel;
import io.netty.channel.unix.DomainSocketAddress;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/** Worker tool grpc server using unix domain sockets */
public class WorkerGrpcServer implements ServerInterceptor {
  private static final Logger LOG = Logger.get(WorkerGrpcServer.class);

  String serverName;
  EventLoopGroup eventLoopGroup;
  Server grpcServer;

  /** main function */
  public static void runServer(
      String serverName, WorkerGrpcService.JvmCDCommandFactory commandFactory) {
    Path workerSocketPath = Paths.get(getRequiredEnvVar("WORKER_SOCKET"));

    try {
      Console console =
          new Console(Verbosity.STANDARD_INFORMATION, System.out, System.err, Ansi.withoutTty());

      try (CompilerDaemonRunner runner =
          new CompilerDaemonRunner(OutputStream.nullOutputStream(), console)) {
        WorkerGrpcService workerService = new WorkerGrpcService(commandFactory, runner);
        WorkerGrpcServer server = new WorkerGrpcServer(serverName, workerSocketPath, workerService);
        server.run();
      }
    } catch (InterruptedException | IOException e) {
      System.err.println(e.getMessage());
      System.exit(1);
    }
    System.exit(0);
  }

  public WorkerGrpcServer(
      String serverName, Path unixDomainSocket, WorkerGrpcService workerService) {
    this.serverName = serverName;
    this.grpcServer = buildServer(workerService, unixDomainSocket);
    Runtime.getRuntime()
        .addShutdownHook(
            new Thread(
                () -> {
                  // Use stderr here since the logger may have been reset by its JVM shutdown hook.
                  System.err.println("*** shutting down gRPC server since JVM is shutting down");
                  stopServer(grpcServer);
                  System.err.println("*** server shut down");
                }));

    Executors.newSingleThreadScheduledExecutor()
        .scheduleAtFixedRate(this::checkCDState, 1, 10, TimeUnit.SECONDS);
  }

  private static final int AVAILABLE_PROCESSORS = Runtime.getRuntime().availableProcessors();

  private boolean isParentAlive() {
    Optional<ProcessHandle> parent = ProcessHandle.current().parent();
    return parent.isPresent() && parent.get().pid() != 1 && parent.get().isAlive();
  }

  private void checkCDState() {
    try {
      logCurrentCDState();
      if (!isParentAlive()) {
        LOG.info("Parent process has been killed, shutting down");
        stopServer(this.grpcServer);
      }
    } catch (Exception e) {
      LOG.error(e);
    }
  }

  private void logCurrentCDState() {
    PerfStatsTracking.MemoryPerfStatsEvent memory = PerfStatsTracking.getMemoryPerfStatsEvent();
    long totalMemoryBytes = memory.getTotalMemoryBytes();
    long freeMemoryBytes = memory.getFreeMemoryBytes();
    long usedMemory = SizeUnit.BYTES.toMegabytes(totalMemoryBytes - freeMemoryBytes);
    long freeMemory = SizeUnit.BYTES.toMegabytes(freeMemoryBytes);
    long totalMemory = SizeUnit.BYTES.toMegabytes(totalMemoryBytes);
    long maxMemory = SizeUnit.BYTES.toMegabytes(memory.getMaxMemoryBytes());
    long timeSpendInGc = TimeUnit.MILLISECONDS.toSeconds(memory.getTimeSpentInGcMs());
    String pools =
        memory.getCurrentMemoryBytesUsageByPool().entrySet().stream()
            .map(e -> e.getKey() + "=" + SizeUnit.BYTES.toMegabytes(e.getValue()))
            .collect(Collectors.joining(", "));

    LOG.info(
        "%s state: Executing tasks: %s, Completed tasks: %s, Available processors: %s, Time spent"
            + " in GC: %s seconds, Used Memory: %s, Free Memory: %s, Total Memory: %s, Max Memory:"
            + " %s, Pools: %s",
        serverName,
        activeTasks.get(),
        completedTasks.get(),
        AVAILABLE_PROCESSORS,
        timeSpendInGc,
        usedMemory,
        freeMemory,
        totalMemory,
        maxMemory,
        pools);
  }

  /** Start the server and wait, clients can connect to the socket after this */
  public void run() throws IOException, InterruptedException {
    LOG.info("Server started");
    grpcServer.start();
    grpcServer.awaitTermination();
  }

  private final AtomicInteger activeTasks = new AtomicInteger(0);
  private final AtomicInteger completedTasks = new AtomicInteger(0);

  @Override
  public <ReqT, RespT> ServerCall.Listener<ReqT> interceptCall(
      ServerCall<ReqT, RespT> call, Metadata headers, ServerCallHandler<ReqT, RespT> next) {
    activeTasks.incrementAndGet();
    return new ForwardingServerCallListener.SimpleForwardingServerCallListener<ReqT>(
        next.startCall(call, headers)) {
      @Override
      public void onComplete() {
        activeTasks.decrementAndGet();
        completedTasks.incrementAndGet();
        super.onComplete();
      }
    };
  }

  private Server buildServer(WorkerGrpcService service, Path unixDomainSocket) {
    LOG.info(
        String.format("Starting %s Worker Server using UDS: %s", serverName, unixDomainSocket));

    DomainSocketAddress socketAddress = new DomainSocketAddress(unixDomainSocket.toString());
    NettyServerBuilder builder =
        NettyServerBuilder.forAddress(socketAddress).addService(service).intercept(this);
    if (KQueue.isAvailable()) {
      this.eventLoopGroup = new KQueueEventLoopGroup();
      builder = builder.channelType(KQueueServerDomainSocketChannel.class);
    } else if (Epoll.isAvailable()) {
      this.eventLoopGroup = new EpollEventLoopGroup();
      builder = builder.channelType(EpollServerDomainSocketChannel.class);
    } else {
      throw new RuntimeException("Server only supports epoll and kqueue (windows not supported)");
    }
    builder = builder.workerEventLoopGroup(eventLoopGroup).bossEventLoopGroup(eventLoopGroup);
    return builder.build();
  }

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
}
