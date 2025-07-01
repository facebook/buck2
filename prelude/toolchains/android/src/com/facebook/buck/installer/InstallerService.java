/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.installer;

import static com.google.common.util.concurrent.MoreExecutors.directExecutor;

import com.facebook.buck.install.model.DeviceMetadata;
import com.facebook.buck.install.model.FileReadyRequest;
import com.facebook.buck.install.model.FileResponse;
import com.facebook.buck.install.model.InstallInfoRequest;
import com.facebook.buck.install.model.InstallResponse;
import com.facebook.buck.install.model.InstallerGrpc;
import com.facebook.buck.install.model.ShutdownRequest;
import com.facebook.buck.install.model.ShutdownResponse;
import com.facebook.buck.util.concurrent.MostExecutors;
import com.facebook.buck.util.types.Unit;
import com.google.common.base.Throwables;
import com.google.common.collect.ImmutableMap;
import com.google.common.util.concurrent.FutureCallback;
import com.google.common.util.concurrent.Futures;
import com.google.common.util.concurrent.ListenableFuture;
import com.google.common.util.concurrent.ListeningExecutorService;
import com.google.common.util.concurrent.MoreExecutors;
import com.google.common.util.concurrent.SettableFuture;
import io.grpc.stub.StreamObserver;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger; // NOPMD
import java.util.stream.Collectors;

/**
 * Installer Service that implements {@code install.proto}
 *
 * <p>The workflow:
 *
 * <ol>
 *   <li>client (buck2) sends `install` request with install id and file map. It could be multiple
 *       targets that would have different install id associated with them.
 *   <li>client sends multiple `fileReady` requests (file data + install id)
 *   <li>{@link InstallerService} tracks all received `fileReady` requests associated with the same
 *       install id. When all files received then {@link InstallerService} calls {@link
 *       InstallCommand} to actually install all received files for the specific install id into the
 *       device/emulator.
 *   <li>client sends `shutdownServer` when wants to stop interactions and asks {@link
 *       InstallerService} to terminate.
 * </ol>
 */
public class InstallerService extends InstallerGrpc.InstallerImplBase {

  // TODO: make configurable or pass from buck2 w/ install info data.
  private static final long INSTALL_MAX_WAIT_TIME = 4;
  private static final TimeUnit INSTALL_TIMEOUT_UNIT = TimeUnit.MINUTES;

  private static final ThreadPoolExecutor THREAD_POOL =
      new ThreadPoolExecutor(
          0,
          Integer.MAX_VALUE,
          1,
          TimeUnit.SECONDS,
          new SynchronousQueue<>(),
          new MostExecutors.NamedThreadFactory("Installer"));

  private static final ListeningExecutorService LISTENING_EXECUTOR_SERVICE =
      MoreExecutors.listeningDecorator(THREAD_POOL);

  private static final Logger LOG = Logger.getLogger(InstallerService.class.getName());
  private final InstallCommand installer;
  private final SettableFuture<Unit> installFinished;
  private final Map<InstallId, Map<String, Optional<Path>>> installIdToFilesMap = new HashMap<>();

  public InstallerService(InstallCommand installer, SettableFuture<Unit> installFinished) {
    this.installer = installer;
    this.installFinished = installFinished;
  }

  @Override
  public void install(
      InstallInfoRequest request, StreamObserver<InstallResponse> responseObserver) {
    try {
      InstallResponse response = handleInstallRequest(request);
      responseObserver.onNext(response);
      responseObserver.onCompleted();
    } catch (Exception e) {
      handleException(responseObserver, e);
    }
  }

  private InstallResponse handleInstallRequest(InstallInfoRequest request) {
    InstallId installId = InstallId.of(request.getInstallId());
    Map<String, String> filesMap = request.getFilesMap();
    LOG.info(
        String.format(
            "Received install id: %s to files map request info: %s",
            installId.getValue(), filesMap));
    synchronized (installIdToFilesMap) {
      installIdToFilesMap.put(
          installId,
          filesMap.keySet().stream()
              .collect(Collectors.toMap(Function.identity(), ignore -> Optional.empty())));
    }
    return InstallResponse.newBuilder().setInstallId(installId.getValue()).build();
  }

  @Override
  public void fileReady(FileReadyRequest request, StreamObserver<FileResponse> responseObserver) {
    try {
      FileResponse fileResponse = handleFileReadyRequest(request);
      responseObserver.onNext(fileResponse);
      responseObserver.onCompleted();
    } catch (Exception e) {
      handleException(responseObserver, e);
    }
  }

  private FileResponse handleFileReadyRequest(FileReadyRequest request)
      throws InterruptedException {
    InstallId installId = InstallId.of(request.getInstallId());
    String name = request.getName();
    String path = request.getPath();
    LOG.info(
        String.format(
            "Received artifact %s located at %s for install id: %s",
            name, path, installId.getValue()));

    FileResponse.Builder fileResponseBuilder =
        FileResponse.newBuilder().setName(name).setPath(path).setInstallId(installId.getValue());

    ImmutableMap<String, Path> installFilesMap;
    synchronized (installIdToFilesMap) {
      Map<String, Optional<Path>> filesMap = installIdToFilesMap.get(installId);
      filesMap.put(name, Optional.of(Paths.get(path)));
      installFilesMap = getFilesMapToInstall(filesMap);
    }

    if (!installFilesMap.isEmpty()) {
      InstallResult installResult = fileReady(installId, installFilesMap);
      if (installResult.isError()) {
        fileResponseBuilder.setErrorDetail(installResult.getInstallError().toProtoModel());
      }
      if (!installResult.getDeviceMetadata().isEmpty()) {
        for (Map<String, String> deviceMetadata : installResult.getDeviceMetadata()) {
          DeviceMetadata.Builder metadataBuilder = DeviceMetadata.newBuilder();
          for (Map.Entry<String, String> entry : deviceMetadata.entrySet()) {
            metadataBuilder.addEntry(
                DeviceMetadata.Entry.newBuilder()
                    .setKey(entry.getKey())
                    .setValue(entry.getValue()));
          }
          fileResponseBuilder.addDeviceMetadata(metadataBuilder);
        }
      }
    }

    return fileResponseBuilder.build();
  }

  private ImmutableMap<String, Path> getFilesMapToInstall(Map<String, Optional<Path>> filesMap) {
    ImmutableMap.Builder<String, Path> installFilesMapBuilder = ImmutableMap.builder();
    boolean allFilesReceived = true;
    for (Map.Entry<String, Optional<Path>> fileEntry : filesMap.entrySet()) {
      String fileName = fileEntry.getKey();
      Optional<Path> pathOptional = fileEntry.getValue();

      if (pathOptional.isEmpty()) {
        allFilesReceived = false;
        break;
      }
      installFilesMapBuilder.put(fileName, pathOptional.get());
    }

    if (allFilesReceived) {
      return installFilesMapBuilder.build();
    }
    return ImmutableMap.of();
  }

  private InstallResult fileReady(InstallId installId, Map<String, Path> filesMap)
      throws InterruptedException {
    LOG.info(String.format("Starting install for install id: %s", installId.getValue()));

    Set<InstallError> installErrors = new HashSet<>();
    CountDownLatch latch = new CountDownLatch(filesMap.size());
    List<ListenableFuture<InstallResult>> futureList = new ArrayList<>(filesMap.size());

    List<Map<String, String>> deviceMetadata;
    for (Map.Entry<String, Path> fileEntry : filesMap.entrySet()) {
      String name = fileEntry.getKey();
      Path path = fileEntry.getValue();

      ListenableFuture<InstallResult> future =
          LISTENING_EXECUTOR_SERVICE.submit(() -> fileReady(name, path, installId));
      Futures.addCallback(
          future, getInstallResultCallback(latch, installErrors, name, path), directExecutor());
      futureList.add(future);
    }

    // wait for all install futures
    boolean allCompleted = latch.await(INSTALL_MAX_WAIT_TIME, INSTALL_TIMEOUT_UNIT);
    if (!allCompleted) {
      // cancel futures
      futureList.forEach(f -> f.cancel(true));
      // return timeout error message
      return new InstallResult(
          List.of(),
          Optional.of(
              new InstallError(
                  "Timeout of "
                      + INSTALL_TIMEOUT_UNIT.toSeconds(INSTALL_MAX_WAIT_TIME)
                      + "s has been exceeded. Install failed.",
                  InfraTimeoutErrorTag.INSTANCE)));
    } else {
      InstallResult allFilesReady = installer.allFilesReady(installId);
      deviceMetadata = allFilesReady.getDeviceMetadata();
      if (allFilesReady.isError()) {
        installErrors.add(allFilesReady.getInstallError());
      }
    }
    InstallResult installResult =
        new InstallResult(deviceMetadata, mergeInstallErrors(installErrors));
    LOG.info("Install [" + installId.getValue() + "] finished with result: " + installResult);
    return installResult;
  }

  private Optional<InstallError> mergeInstallErrors(Set<InstallError> installErrors) {
    Set<InstallErrorTag> allTags = new HashSet<>();
    List<String> allMessages = new ArrayList<>();
    for (InstallError installError : installErrors) {
      allTags.addAll(installError.getTags());
      allMessages.add(installError.getMessage());
    }

    return allMessages.isEmpty()
        ? Optional.empty()
        : Optional.of(
            new InstallError(
                allMessages.stream()
                    .collect(
                        Collectors.joining(
                            allMessages.size() > 1 ? "\n> " : "\n",
                            allMessages.size() > 1 ? "\n> " : "",
                            "")),
                allTags));
  }

  private FutureCallback<InstallResult> getInstallResultCallback(
      CountDownLatch latch, Set<InstallError> installErrors, String name, Path path) {
    return new FutureCallback<>() {
      @Override
      public void onSuccess(InstallResult installResult) {
        if (installResult.isError()) {
          InstallError installError = installResult.getInstallError();
          LOG.info(
              String.format(
                  "Installation of file name: %s and path: %s failed with error: %s",
                  name, path, installError));
          synchronized (installErrors) {
            installErrors.add(installError);
          }
        }
        latch.countDown();
      }

      @Override
      public void onFailure(Throwable thrown) {
        String stackTraceAsString = Throwables.getStackTraceAsString(thrown);
        LOG.info(
            String.format(
                "Installation of file name: %s and path: %s failed with error: %s",
                name, path, stackTraceAsString));
        synchronized (installErrors) {
          installErrors.add(new InstallError(stackTraceAsString, DefaultInstallErrorTag.INSTANCE));
        }
        latch.countDown();
      }
    };
  }

  private InstallResult fileReady(String name, Path path, InstallId installId) {
    return installer.fileReady(name, path, installId);
  }

  @Override
  public void shutdownServer(
      ShutdownRequest request, StreamObserver<ShutdownResponse> responseObserver) {
    try {
      handleShutdownServerRequest(responseObserver);
    } catch (Exception e) {
      handleException(responseObserver, e);
    }
  }

  private void handleShutdownServerRequest(StreamObserver<ShutdownResponse> responseObserver) {
    LOG.info("Received shutting down request");
    responseObserver.onNext(ShutdownResponse.getDefaultInstance());
    responseObserver.onCompleted();
    installFinished.set(Unit.UNIT);
  }

  private void handleException(StreamObserver<?> responseObserver, Exception e) {
    LOG.log(Level.SEVERE, "Unexpected exception", e);
    responseObserver.onError(
        io.grpc.Status.INTERNAL
            .withDescription("Unexpected exception: " + Throwables.getStackTraceAsString(e))
            .asException());
  }
}
