/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.exopackage;

import com.facebook.buck.android.AdbHelper;
import com.facebook.buck.android.AndroidInstallPrinter;
import com.facebook.buck.android.IsolatedApkInfo;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.installer.android.AndroidInstallException;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.facebook.buck.util.NamedTemporaryFile;
import com.facebook.infer.annotation.Nullsafe;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Preconditions;
import com.google.common.base.Splitter;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSortedMap;
import com.google.common.collect.ImmutableSortedSet;
import com.google.common.collect.Ordering;
import com.google.common.io.Closer;
import com.google.common.util.concurrent.ThreadFactoryBuilder;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalLong;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.function.Function;

/** ExopackageInstaller manages the installation of apps with the "exopackage" flag set to true. */
@Nullsafe(Nullsafe.Mode.LOCAL)
public class ExopackageInstaller {

  private static final Logger LOG = Logger.get(ExopackageInstaller.class);

  // When there are a small number of files to delete, it's faster (we issue
  // fewer rm commands over adb) if we group them by the data root, so
  //   cd /data/local/tmp/exopackage/foo && rm resources/bar.txt native-libs/baz.txt
  // But when there are more files, because of the way RealAndroidDevice splits up
  // commandlines to avoid hitting limits, it's more efficient to group by the
  // subdirectories, so
  //   cd /data/local/tmp/exopackage/foo/resources && rm bar.txt bap.txt boz.txt
  //   cd /data/local/tmp/exopackage/foo/native-libs && rm baz.txt zog.txt
  // We pick a heuristic number of files (10) at which to change behavior for this based
  // on these assumptions:
  //    approx available commandline length = 800
  //    max length of a path from the dataRoot for a well known app = 77
  private static final int RM_GROUPING_THRESHOLD = 10;

  private static final long BYTES_PER_BLOCK = 1024L;

  private static final int MAX_CONCURRENT_PUSHES = 8;

  private static final long TARGET_SHARD_BYTES = 32L * 1024L * 1024L;

  private static final ExecutorService PUSH_EXECUTOR =
      Executors.newFixedThreadPool(
          MAX_CONCURRENT_PUSHES,
          new ThreadFactoryBuilder().setNameFormat("exopackage-push-%d").setDaemon(true).build());

  public static final Path EXOPACKAGE_INSTALL_ROOT = Paths.get("/data/local/tmp/exopackage/");

  private final IsolatedExopackageInfo exoInfo;
  private final AndroidInstallPrinter androidPrinter;
  private final AbsPath rootPath;
  private final AndroidDevice device;
  private final String packageName;
  private final Optional<String> buck2BuildUuid;
  private final Path dataRoot;
  private final boolean skipMetadataIfNoInstalls;
  private final InstallTimings timings;

  public ExopackageInstaller(
      IsolatedExopackageInfo exoInfo,
      AndroidInstallPrinter androidPrinter,
      AbsPath rootPath,
      String packageName,
      AndroidDevice device,
      boolean skipMetadataIfNoInstalls,
      Optional<String> buck2BuildUuid) {
    this(
        exoInfo,
        androidPrinter,
        rootPath,
        packageName,
        device,
        skipMetadataIfNoInstalls,
        buck2BuildUuid,
        InstallTimings.NONE);
  }

  public ExopackageInstaller(
      IsolatedExopackageInfo exoInfo,
      AndroidInstallPrinter androidPrinter,
      AbsPath rootPath,
      String packageName,
      AndroidDevice device,
      boolean skipMetadataIfNoInstalls,
      Optional<String> buck2BuildUuid,
      InstallTimings timings) {
    this.timings = timings;
    this.exoInfo = exoInfo;
    this.androidPrinter = androidPrinter;
    this.rootPath = rootPath;
    this.device = device;
    this.packageName = packageName;
    this.dataRoot = EXOPACKAGE_INSTALL_ROOT.resolve(packageName);
    this.skipMetadataIfNoInstalls = skipMetadataIfNoInstalls;
    this.buck2BuildUuid = buck2BuildUuid;

    Preconditions.checkArgument(AdbHelper.PACKAGE_NAME_PATTERN.matcher(packageName).matches());
  }

  /** Installs an apk, restarting the running app if necessary. */
  public void doInstall(IsolatedApkInfo isolatedApkInfo, SetDebugAppMode setDebugAppMode)
      throws Exception {
    if (setDebugAppMode == SetDebugAppMode.SET) {
      device.setDebugAppPackageName(packageName);
    }
    if (exopackageEnabled()) {
      long setupStart = System.currentTimeMillis();
      device.mkDirP(dataRoot.toString());
      device.fixRootDir(dataRoot.toString());
      ImmutableSortedSet<Path> presentFiles = device.listDirRecursive(dataRoot);
      timings.recordDeviceSetup(setupStart, System.currentTimeMillis());
      ImmutableList<ResolvedExoPayload> payloads = resolveExoPayloads();

      // Reclaim space before pushing so a device that is already full can free room for the
      // payload. Best effort on purpose: neither step is needed for the install to be correct.
      // Scratch is read by nothing, and the unwanted set is disjoint from the push set, so the new
      // app gets every file it needs either way -- a failure here leaves unreferenced files on the
      // device and costs disk, not correctness. Whether there is still room to proceed is the
      // preflight's answer to give, from the space the device actually has.
      try {
        device.rmStaleFiles(packageName);
        deleteUnwantedFiles(presentFiles, wantedPaths(payloads));
      } catch (Exception e) {
        LOG.warn(e, "Could not reclaim exopackage space for %s; continuing", packageName);
      }
      checkEnoughFreeSpace(presentFiles, payloads);

      installMissingExopackageFiles(presentFiles, payloads);
    }
    if (buck2BuildUuid.isPresent()) {
      device.installBuildUuidFile(
          AdbHelper.BUILD_METADATA_INSTALL_ROOT, packageName, buck2BuildUuid.get());
    }
    installAndRestartApk(isolatedApkInfo);
  }

  public void installAndRestartApk(IsolatedApkInfo isolatedApkInfo) throws Exception {
    installApkIfNecessary(isolatedApkInfo);
    killApp();
  }

  private void installApkIfNecessary(IsolatedApkInfo isolatedApkInfo) throws Exception {
    File apk = isolatedApkInfo.getApkPath().toFile();

    if (shouldAppBeInstalled(isolatedApkInfo)) {
      long installStart = System.currentTimeMillis();
      boolean success =
          device.installApkOnDevice(
              apk,
              /* installViaSd= */ false,
              /* quiet */ false,
              /* verifyTempWritable= */ true,
              /* stagedInstallMode= */ false,
              /* userId= */ null,
              /* allowFastDeploy= */ false,
              packageName);
      timings.recordApkInstall(installStart, System.currentTimeMillis());
      if (!success) {
        throw AndroidInstallException.Companion.adbCommandFailedException(
            "Installing Apk failed.", null);
      }
    }
  }

  private void killApp() throws Exception {
    device.stopPackage(packageName);
  }

  /**
   * The exopackage payloads for this install, with their contents resolved exactly once.
   *
   * <p>Resolving a payload parses its metadata file and, for native libs, queries the device for
   * its ABIs. The delete and push phases both need the same answers, so they share one resolution
   * rather than recomputing it.
   */
  private ImmutableList<ResolvedExoPayload> resolveExoPayloads() throws Exception {
    ImmutableList.Builder<ResolvedExoPayload> payloads = ImmutableList.builder();

    Optional<IsolatedExopackageInfo.IsolatedDexInfo> dexInfo = exoInfo.getDexInfo();
    if (dexInfo.isPresent()) {
      payloads.add(new ResolvedExoPayload(new DexExoHelper(rootPath, dexInfo.get())));
    }

    Optional<IsolatedExopackageInfo.IsolatedNativeLibsInfo> nativeLibsInfo =
        exoInfo.getNativeLibsInfo();
    if (nativeLibsInfo.isPresent()) {
      payloads.add(
          new ResolvedExoPayload(
              new NativeExoHelper(this::getDeviceAbis, rootPath, nativeLibsInfo.get())));
    }

    Optional<IsolatedExopackageInfo.IsolatedResourcesInfo> resourcesInfo =
        exoInfo.getResourcesInfo();
    if (resourcesInfo.isPresent()) {
      payloads.add(new ResolvedExoPayload(new ResourcesExoHelper(rootPath, resourcesInfo.get())));
    }

    return payloads.build();
  }

  private List<String> getDeviceAbis() {
    try {
      return device.getDeviceAbis();
    } catch (Exception e) {
      throw AndroidInstallException.Companion.adbCommandFailedException(
          "Unable to communicate with device.", e.getMessage());
    }
  }

  /** Every path this install wants on the device; anything else under the data root is stale. */
  private static ImmutableSet<Path> wantedPaths(ImmutableList<ResolvedExoPayload> payloads) {
    ImmutableSet.Builder<Path> wantedPaths = ImmutableSet.builder();
    for (ResolvedExoPayload payload : payloads) {
      wantedPaths.addAll(payload.filesToInstall.keySet());
      wantedPaths.addAll(payload.metadataToInstall.keySet());
    }
    return wantedPaths.build();
  }

  /** Installs missing exo package files */
  private void installMissingExopackageFiles(
      ImmutableSortedSet<Path> presentFiles, ImmutableList<ResolvedExoPayload> payloads)
      throws Exception {
    ImmutableMap.Builder<Path, String> metadata = ImmutableMap.builder();
    ImmutableList.Builder<PushShard> shards = ImmutableList.builder();
    for (ResolvedExoPayload payload : payloads) {
      ImmutableSortedMap<Path, Path> filesToInstall =
          payload.filesToInstall.entrySet().stream()
              .filter(entry -> !presentFiles.contains(entry.getKey()))
              .collect(
                  ImmutableSortedMap.toImmutableSortedMap(
                      Ordering.natural(), Map.Entry::getKey, Map.Entry::getValue));
      shards.addAll(
          splitIntoShards(payload.type, filesToInstall, rootPath, dataRoot, TARGET_SHARD_BYTES));
      if (!skipMetadataIfNoInstalls || !filesToInstall.isEmpty()) {
        metadata.putAll(payload.metadataToInstall);
      }
    }

    pushShards(shards.build());

    // Metadata is what the app reads to find these files, so it must not land before them.
    installMetadata(metadata.build());
  }

  /**
   * Splits a payload into roughly equal chunks by size.
   *
   * <p>A single `adb push` is limited by per-stream round trips rather than by bandwidth or by the
   * host, so concurrent pushes scale close to linearly. Sharding by bytes rather than by payload
   * matters because native libs alone are over half the total, and pushing one payload per stream
   * leaves that stream setting the wall time on its own.
   */
  @VisibleForTesting
  static ImmutableList<PushShard> splitIntoShards(
      String filesType,
      ImmutableSortedMap<Path, Path> filesToInstall,
      AbsPath rootPath,
      Path dataRoot,
      long targetShardBytes) {
    ImmutableList.Builder<PushShard> shards = ImmutableList.builder();
    ImmutableMap.Builder<Path, Path> current = ImmutableMap.builder();
    long currentBytes = 0L;
    boolean currentIsEmpty = true;

    for (Map.Entry<Path, Path> file : filesToInstall.entrySet()) {
      Path localPath = rootPath.resolve(file.getValue()).getPath();
      current.put(dataRoot.resolve(file.getKey()), localPath);
      currentBytes += localPath.toFile().length();
      currentIsEmpty = false;
      // A shard can never be smaller than a single file, so a payload of one huge file stays whole.
      if (currentBytes >= targetShardBytes) {
        shards.add(new PushShard(filesType, current.build()));
        current = ImmutableMap.builder();
        currentBytes = 0L;
        currentIsEmpty = true;
      }
    }
    if (!currentIsEmpty) {
      shards.add(new PushShard(filesType, current.build()));
    }
    return shards.build();
  }

  /** Pushes every shard, up to {@link #MAX_CONCURRENT_PUSHES} at a time. */
  private void pushShards(ImmutableList<PushShard> shards) throws Exception {
    if (shards.isEmpty()) {
      return;
    }
    try (AutoCloseable ignored = device.createForward()) {
      // Create every destination directory up front: shards from one payload share a directory,
      // and concurrent mkdir -p of the same path is pointless work at best.
      ImmutableSet<Path> destinationDirs =
          shards.stream()
              .flatMap(shard -> shard.installPaths.keySet().stream())
              .map(Path::getParent)
              .filter(Objects::nonNull)
              .collect(ImmutableSet.toImmutableSet());
      for (Path destinationDir : destinationDirs) {
        device.mkDirP(destinationDir.toString());
      }

      List<Future<Void>> pushes = new ArrayList<>(shards.size());
      for (PushShard shard : shards) {
        pushes.add(
            PUSH_EXECUTOR.submit(
                () -> {
                  pushShard(shard);
                  return null;
                }));
      }
      awaitAll(pushes);
    }
  }

  private void pushShard(PushShard shard) throws Exception {
    long start = System.currentTimeMillis();
    device.installFiles(shard.filesType, shard.installPaths, packageName);
    // Each shard records its own window; the group's transfer time is their union.
    timings.recordPush(shard.filesType, start, System.currentTimeMillis());
  }

  /**
   * Waits for every push, reporting the first failure with any others attached.
   *
   * <p>Deliberately does not cancel the rest once one fails. A thread blocked reading an {@code
   * adb} subprocess does not observe an interrupt, so cancelling would not stop the transfer -- it
   * would only stop us waiting for it, leaving shards writing to the device after the install has
   * been called failed, and skipping the scratch each one removes on its way out. The cost is that
   * a failing install takes as long as its slowest shard.
   *
   * <p>Interruption is the exception: it means the process is going away, so it stops waiting and
   * leaves whatever is in flight to die with the JVM.
   */
  private static void awaitAll(List<Future<Void>> pushes) throws Exception {
    Exception failure = null;
    for (Future<Void> push : pushes) {
      try {
        push.get();
      } catch (ExecutionException e) {
        Exception cause = e.getCause() instanceof Exception ? (Exception) e.getCause() : e;
        if (failure == null) {
          failure = cause;
        } else {
          // Every shard that failed, not just the first: they fail independently, and which one
          // arrives first says nothing about which one explains the install.
          failure.addSuppressed(cause);
        }
      } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
        throw e;
      }
    }
    if (failure != null) {
      throw failure;
    }
  }

  /** One concurrently pushable chunk of a payload. */
  @VisibleForTesting
  static final class PushShard {
    final String filesType;
    final ImmutableMap<Path, Path> installPaths;

    PushShard(String filesType, ImmutableMap<Path, Path> installPaths) {
      this.filesType = filesType;
      this.installPaths = installPaths;
    }
  }

  /**
   * Fails the install if the payload cannot fit, rather than letting the push die partway through
   * with a bare ENOSPC.
   */
  private void checkEnoughFreeSpace(
      ImmutableSortedSet<Path> presentFiles, ImmutableList<ResolvedExoPayload> payloads) {
    OptionalLong availableBytes = availableBytesOnDevice();
    if (availableBytes.isEmpty()) {
      return;
    }
    long requiredBytes = 0L;
    for (ResolvedExoPayload payload : payloads) {
      for (Map.Entry<Path, Path> file : payload.filesToInstall.entrySet()) {
        if (!presentFiles.contains(file.getKey())) {
          File source = rootPath.resolve(file.getValue()).toFile();
          if (!source.isFile()) {
            // Zero is what length() would answer, which would quietly shrink the estimate and let
            // the check pass. The install cannot succeed without the file either way, so say which
            // one is missing while there is still somewhere useful to say it.
            throw AndroidInstallException.Companion.artifactMissing(source.toString());
          }
          requiredBytes += source.length();
        }
      }
    }
    if (requiredBytes > availableBytes.getAsLong()) {
      throw AndroidInstallException.Companion.insufficientStorage(
          requiredBytes, availableBytes.getAsLong());
    }
  }

  /**
   * Free space under the data partition, or empty if the device did not give a number for it.
   *
   * <p>Unsuffixed, {@link AndroidDevice#getDiskSpace} answers with three entries -- size, used,
   * available -- each a count of 1K blocks, e.g. {@code ["32911312", "14799512", "17964344"]}. A
   * device it cannot read reports {@code "_"} in their place.
   */
  private OptionalLong availableBytesOnDevice() {
    List<String> diskSpace = device.getDiskSpace(/* humanReadable= */ false);
    if (diskSpace.size() < 3) {
      return OptionalLong.empty();
    }
    String availableBlocks = diskSpace.get(2).trim();
    try {
      return OptionalLong.of(Long.parseLong(availableBlocks) * BYTES_PER_BLOCK);
    } catch (NumberFormatException e) {
      // No number means nothing to check against, so the preflight is skipped rather than guessed
      // at, and the install goes on to fail at the push if the space really is not there.
      LOG.info("Could not read available device space from '%s'", availableBlocks);
      return OptionalLong.empty();
    }
  }

  /** One exopackage payload class, with its contents resolved exactly once. */
  private static final class ResolvedExoPayload {
    private final String type;
    private final ImmutableMap<Path, Path> filesToInstall;
    private final ImmutableMap<Path, String> metadataToInstall;

    ResolvedExoPayload(ExoHelper helper) throws IOException {
      this.type = helper.getType();
      this.filesToInstall = helper.getFilesToInstall();
      this.metadataToInstall = helper.getMetadataToInstall();
    }
  }

  /**
   * @return true if the given apk info contains any items which need to be installed via exopackage
   */
  private boolean exopackageEnabled() {
    return isExopackage(exoInfo);
  }

  /** True when {@code exoInfo} carries any payload to be pushed alongside the apk. */
  public static boolean isExopackage(IsolatedExopackageInfo exoInfo) {
    return exoInfo.getDexInfo().isPresent()
        || exoInfo.getNativeLibsInfo().isPresent()
        || exoInfo.getResourcesInfo().isPresent();
  }

  private Optional<PackageInfo> getPackageInfo(String packageName) throws Exception {
    return device.getPackageInfo(packageName);
  }

  private boolean shouldAppBeInstalled(IsolatedApkInfo apkInfo) throws Exception {
    Optional<PackageInfo> appPackageInfo = getPackageInfo(packageName);
    if (appPackageInfo.isEmpty()) {
      androidPrinter.printMessage("App not installed.  Installing now.");
      return true;
    }

    LOG.debug("App path: %s", appPackageInfo.get().apkPath);
    String installedAppManifestDigest = getInstalledAppManifestDigest(appPackageInfo.get().apkPath);
    String localAppManifestDigest =
        ExopackageUtil.getJarManifestDigest(apkInfo.getApkPath().toString());
    LOG.info("Local APK manifest digest: %s", localAppManifestDigest);
    LOG.info("Installed APK manifest digest: %s", installedAppManifestDigest);

    if (!installedAppManifestDigest.equals(localAppManifestDigest)) {
      LOG.info("APK manifest digests do not match.  Must re-install.");
      return true;
    }

    LOG.info("APK manifest digests match.  No need to install.");
    return false;
  }

  private String getInstalledAppManifestDigest(String packagePath) throws Exception {
    String output = device.getApkManifestDigest(packagePath);

    String result = output.trim();
    if (result.contains("\n") || result.contains("\r")) {
      throw new IllegalStateException("Unexpected APK manifest digest:\n" + output);
    }

    return result;
  }

  private void deleteUnwantedFiles(
      ImmutableSortedSet<Path> presentFiles, ImmutableSet<Path> wantedFiles) {
    ImmutableSortedSet<Path> filesToDelete =
        presentFiles.stream()
            .filter(p -> !p.getFileName().toString().equals("lock") && !wantedFiles.contains(p))
            .collect(ImmutableSortedSet.toImmutableSortedSet(Ordering.natural()));
    deleteFiles(filesToDelete);
  }

  private void deleteFiles(ImmutableSortedSet<Path> filesToDelete) {
    Function<Path, Path> toRootDirFn =
        filesToDelete.size() <= RM_GROUPING_THRESHOLD
            ? path -> dataRoot
            : path -> dataRoot.resolve(path).getParent();
    Function<Path, String> toFileFn =
        filesToDelete.size() <= RM_GROUPING_THRESHOLD
            ? Path::toString
            : path -> path.getFileName().toString();

    try {
      filesToDelete.stream()
          .collect(ImmutableListMultimap.toImmutableListMultimap(toRootDirFn, toFileFn))
          .asMap()
          .forEach((dir, files) -> device.rmFiles(dir.toString(), files));
    } catch (Exception e) {
      // Every failure, not just a tagged one: `rmFiles` also surfaces the IO of writing and
      // pushing its manifest, and those would otherwise escape without the tag below.
      throw AndroidInstallException.Companion.exopackageGarbageCollectionFailed(e.getMessage());
    }
  }

  private void installMetadata(ImmutableMap<Path, String> metadataToInstall) throws Exception {
    try (Closer closer = Closer.create()) {
      ImmutableMap.Builder<Path, Path> filesToInstall = ImmutableMap.builder();
      for (Map.Entry<Path, String> entry : metadataToInstall.entrySet()) {
        NamedTemporaryFile temp =
            Objects.requireNonNull(closer.register(new NamedTemporaryFile("metadata", "tmp")));
        com.google.common.io.Files.write(
            entry.getValue().getBytes(StandardCharsets.UTF_8), temp.get().toFile());
        filesToInstall.put(
            dataRoot.resolve(entry.getKey()), rootPath.resolve(temp.get()).getPath());
      }
      // Pushed as one shard, through the same path as every payload. It stays inside the closer:
      // the temporary files it names are deleted when that closes.
      pushShards(ImmutableList.of(new PushShard("metadata", filesToInstall.build())));
    }
  }

  /**
   * Parses a text file which is supposed to be in the following format: "file_path_without_spaces
   * file_hash ...." i.e. it parses the first two columns of each line and ignores the rest of it.
   *
   * @return A multi map from the file hash to its path, which equals the raw path resolved against
   *     {@code resolvePathAgainst}.
   */
  @VisibleForTesting
  public static ImmutableMultimap<String, Path> parseExopackageInfoMetadata(
      Path metadataTxt, Path resolvePathAgainst, AbsPath rootPath) throws IOException {
    ImmutableMultimap.Builder<String, Path> builder = ImmutableMultimap.builder();
    for (String line : ProjectFilesystemUtils.readLines(rootPath, metadataTxt)) {
      // ignore lines that start with '.'
      if (line.startsWith(".")) {
        continue;
      }
      List<String> parts = Splitter.on(' ').splitToList(line);
      if (parts.size() < 2) {
        throw new RuntimeException("Illegal line in metadata file: " + line);
      }
      builder.put(parts.get(1), resolvePathAgainst.resolve(parts.get(0)));
    }
    return builder.build();
  }

  public static ImmutableMultimap<String, Path> parseExopackageInfoMetadata(
      AbsPath metadataTxt, AbsPath resolvePathAgainst, AbsPath rootPath) throws IOException {
    return parseExopackageInfoMetadata(
        metadataTxt.getPath(), resolvePathAgainst.getPath(), rootPath);
  }
}
