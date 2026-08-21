/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.installer.android;

import com.facebook.buck.android.exopackage.InstallTimings;
import com.facebook.buck.core.filesystems.AbsPath;
import com.google.common.collect.ImmutableSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import javax.annotation.Nullable;

/**
 * Holds android install related artifacts (apk options, manifest path, etc)
 *
 * <p>Shared across the gRPC handler threads that deliver artifacts, the threads that push them, and
 * the install that reads them, so every member is guarded by this object's monitor.
 */
class AndroidArtifacts implements InstallTimings {
  private AbsPath androidManifestPath;
  private AndroidInstallApkOptions apkOptions;
  private AbsPath apk;
  private ImmutableSet<String> apkAbis;
  private Optional<AbsPath> secondaryDexExopackageInfoDirectory = Optional.empty();
  private Optional<AbsPath> secondaryDexExopackageInfoMetadata = Optional.empty();
  private Optional<AbsPath> nativeLibraryExopackageInfoDirectory = Optional.empty();
  private Optional<AbsPath> nativeLibraryExopackageInfoMetadata = Optional.empty();
  private Optional<AbsPath> resourcesExopackageInfoAssets = Optional.empty();
  private Optional<AbsPath> resourcesExopackageInfoAssetsHash = Optional.empty();
  private Optional<AbsPath> resourcesExopackageInfoRes = Optional.empty();
  private Optional<AbsPath> resourcesExopackageInfoResHash = Optional.empty();
  // Artifact name -> wall-clock arrival.
  private final Map<String, Long> fileArrivalMillis = new HashMap<>();

  /**
   * Records that {@code artifactName} was delivered by buck at {@code timestampMillis}. Must be
   * called when the file is actually received, not when the installer gets around to consuming it,
   * or every artifact is stamped with the same instant and the spans below all collapse to zero.
   */
  public synchronized void recordFileArrival(String artifactName, long timestampMillis) {
    fileArrivalMillis.putIfAbsent(artifactName, timestampMillis);
  }

  // Stage timings, written from the push threads and read once the install is over.
  // Every shard's window, per push group. Kept separately rather than spanned: shards of a group
  // overlap each other and queue behind other groups, so only the union of these is time the group
  // was actually transferring.
  private final Map<String, List<long[]>> pushWindows = new LinkedHashMap<>();
  private long deviceSetupMillis;
  private long apkInstallMillis;
  private long deviceWorkMillis;

  @Override
  public synchronized void recordDeviceSetup(long startMillis, long endMillis) {
    deviceSetupMillis += endMillis - startMillis;
  }

  @Override
  public synchronized void recordPush(String group, long startMillis, long endMillis) {
    pushWindows
        .computeIfAbsent(group, unused -> new ArrayList<>())
        .add(new long[] {startMillis, endMillis});
  }

  @Override
  public synchronized void recordApkInstall(long startMillis, long endMillis) {
    apkInstallMillis += endMillis - startMillis;
  }

  @Override
  public synchronized void recordDeviceWork(long startMillis, long endMillis) {
    deviceWorkMillis += endMillis - startMillis;
  }

  /**
   * How the install went, as one timeline. Every value is seconds.milliseconds, measured from the
   * first artifact arriving.
   *
   * <p>Each class of artifact reports when buck finished delivering it ({@code _arrival_s}) and how
   * long moving it to the device took ({@code _transfer_s}), in arrival order. Control artifacts
   * are read on the host, so they never transfer.
   *
   * <p>{@code critical_path_s} replays this same install against these same durations, but starts
   * each payload the moment its own artifacts were there instead of waiting for all of them. {@code
   * potential_saving_s} is {@code total_s} minus that: time the device spent waiting for an
   * artifact it did not yet need. Neither says anything about transferring faster, only about
   * waiting less.
   */
  public synchronized Map<String, String> getInstallMetrics(long installCompleteMillis) {
    if (fileArrivalMillis.isEmpty()) {
      return Map.of();
    }
    long first = Collections.min(fileArrivalMillis.values());

    Map<ArtifactClass, Long> readyByClass = new EnumMap<>(ArtifactClass.class);
    for (Map.Entry<String, Long> arrival : fileArrivalMillis.entrySet()) {
      readyByClass.merge(ArtifactClass.of(arrival.getKey()), arrival.getValue(), Math::max);
    }
    long controlReady = readyByClass.getOrDefault(ArtifactClass.CONTROL, first);
    long apkReady = readyByClass.getOrDefault(ArtifactClass.APK, first);

    // Replay: one pusher, each payload started as soon as it was available.
    List<long[]> payloads = new ArrayList<>(); // {availableAt, durationMillis}
    long metadataMillis = 0L;
    for (Map.Entry<String, List<long[]>> push : pushWindows.entrySet()) {
      long duration = unionMillis(push.getValue());
      ArtifactClass pushed = ArtifactClass.forPushGroup(push.getKey());
      if (pushed == null) {
        metadataMillis += duration; // metadata is derived, so it can only follow every payload
        continue;
      }
      long availableAt = readyByClass.getOrDefault(pushed, controlReady);
      payloads.add(new long[] {Math.max(availableAt, controlReady), duration});
    }
    payloads.sort(Comparator.comparingLong(payload -> payload[0]));

    // Replay onto as many pushers as the install actually used, so the estimate tracks the
    // implementation. With one pusher this is a queue; with several it is a list schedule.
    long[] pushers = new long[observedConcurrency()];
    Arrays.fill(pushers, controlReady + deviceSetupMillis);
    for (long[] payload : payloads) {
      int earliest = 0;
      for (int i = 1; i < pushers.length; i++) {
        if (pushers[i] < pushers[earliest]) {
          earliest = i;
        }
      }
      pushers[earliest] = Math.max(pushers[earliest], payload[0]) + payload[1];
    }
    long clock = controlReady + deviceSetupMillis;
    for (long pusher : pushers) {
      clock = Math.max(clock, pusher);
    }
    clock += metadataMillis;
    clock = Math.max(clock, apkReady) + apkInstallMillis;
    // Device work that is not one of the stages above still has to happen, so charge it too.
    // Without this the replay looks faster than anything achievable and every install appears to
    // have a saving.
    long modelled = deviceSetupMillis + apkInstallMillis;
    for (List<long[]> windows : pushWindows.values()) {
      modelled += unionMillis(windows);
    }
    clock += Math.max(0L, deviceWorkMillis - modelled);

    long total = installCompleteMillis - first;
    long criticalPath = clock - first;

    Map<String, String> metrics = new LinkedHashMap<>();
    readyByClass.entrySet().stream()
        .sorted(Map.Entry.comparingByValue())
        .forEach(
            ready -> {
              String artifactClass = ready.getKey().metricName;
              metrics.put(artifactClass + "_arrival_s", seconds(ready.getValue() - first));
              metrics.put(artifactClass + "_transfer_s", seconds(transferMillis(ready.getKey())));
            });
    metrics.put("device_setup_s", seconds(deviceSetupMillis));
    if (metadataMillis > 0L) {
      metrics.put("metadata_transfer_s", seconds(metadataMillis));
    }
    metrics.put("critical_path_s", seconds(criticalPath));
    metrics.put("total_s", seconds(total));
    metrics.put("potential_saving_s", seconds(Math.max(0L, total - criticalPath)));
    return metrics;
  }

  /** How long this class of artifact took to reach the device, or zero if it is not transferred. */
  private long transferMillis(ArtifactClass artifactClass) {
    if (artifactClass == ArtifactClass.APK) {
      return apkInstallMillis;
    }
    for (Map.Entry<String, List<long[]>> push : pushWindows.entrySet()) {
      if (ArtifactClass.forPushGroup(push.getKey()) == artifactClass) {
        return unionMillis(push.getValue());
      }
    }
    return 0L;
  }

  /**
   * Time a group spent transferring: the union of its shards' windows. Overlapping shards count
   * once, and a gap where every shard was queued behind another group counts for nothing -- the
   * span between the first start and the last end would charge the group for both.
   */
  private static long unionMillis(List<long[]> windows) {
    long total = 0L;
    for (long[] window : coalesce(windows)) {
      total += window[1] - window[0];
    }
    return total;
  }

  /** The windows in order, with overlapping ones fused so each instant appears once. */
  private static List<long[]> coalesce(List<long[]> windows) {
    List<long[]> sorted = new ArrayList<>(windows);
    sorted.sort(Comparator.comparingLong(window -> window[0]));
    List<long[]> fused = new ArrayList<>();
    for (long[] window : sorted) {
      long[] open = fused.isEmpty() ? null : fused.get(fused.size() - 1);
      if (open != null && window[0] <= open[1]) {
        open[1] = Math.max(open[1], window[1]);
      } else {
        fused.add(new long[] {window[0], window[1]});
      }
    }
    return fused;
  }

  private static String seconds(long millis) {
    return String.format(Locale.ROOT, "%.3f", millis / 1000.0);
  }

  /**
   * How many payloads the install had in flight at once, from their observed windows. One if the
   * pushes did not overlap.
   */
  private int observedConcurrency() {
    List<long[]> edges = new ArrayList<>();
    for (List<long[]> windows : pushWindows.values()) {
      // Fused first: a group's own shards overlapping is not two payloads in flight.
      for (long[] window : coalesce(windows)) {
        if (window[1] > window[0]) {
          edges.add(new long[] {window[0], 1L});
          edges.add(new long[] {window[1], -1L});
        }
      }
    }
    // Ends before starts at the same instant, so touching windows are not counted as overlapping.
    edges.sort(
        Comparator.<long[]>comparingLong(edge -> edge[0]).thenComparingLong(edge -> edge[1]));
    int concurrent = 0;
    int peak = 0;
    for (long[] edge : edges) {
      concurrent += (int) edge[1];
      peak = Math.max(peak, concurrent);
    }
    return Math.max(1, peak);
  }

  /** Groups install artifacts by the exopackage payload they belong to. */
  private enum ArtifactClass {
    SECONDARY_DEX("dex"),
    NATIVE_LIBRARY("native"),
    RESOURCES("resources"),
    CONTROL("control"),
    APK("apk");

    final String metricName;

    ArtifactClass(String metricName) {
      this.metricName = metricName;
    }

    /** The class whose arrival gates pushing {@code pushGroup}, or null if it is derived. */
    @Nullable
    static ArtifactClass forPushGroup(String pushGroup) {
      switch (pushGroup) {
        case "secondary_dex":
          return SECONDARY_DEX;
        case "native_library":
          return NATIVE_LIBRARY;
        case "resources":
          return RESOURCES;
        default:
          return null;
      }
    }

    static ArtifactClass of(String artifactName) {
      if (artifactName.startsWith("secondary_dex")) {
        return SECONDARY_DEX;
      }
      if (artifactName.startsWith("native_library")) {
        return NATIVE_LIBRARY;
      }
      if (artifactName.startsWith("resources_exopackage")) {
        return RESOURCES;
      }
      if (artifactName.equals("options")
          || artifactName.equals("manifest")
          || artifactName.equals("cpu_filters")) {
        return CONTROL;
      }
      // AndroidInstallerManager#fileReady treats any unrecognised name as the apk.
      return APK;
    }
  }

  public synchronized void setAndroidManifestPath(AbsPath androidManifestPath) {
    this.androidManifestPath = androidManifestPath;
  }

  public synchronized AbsPath getAndroidManifestPath() {
    return this.androidManifestPath;
  }

  public synchronized void setApkOptions(AndroidInstallApkOptions apkOptions) {
    this.apkOptions = apkOptions;
  }

  public synchronized AndroidInstallApkOptions getApkOptions() {
    return this.apkOptions;
  }

  /**
   * The ABIs the apk carries native code for, or null until buck has sent the cpu filters. Empty
   * means the filters named nothing this installer recognises.
   */
  @Nullable
  public synchronized ImmutableSet<String> getApkAbis() {
    return apkAbis;
  }

  public synchronized void setApkAbis(ImmutableSet<String> apkAbis) {
    this.apkAbis = apkAbis;
  }

  public synchronized AbsPath getApk() {
    return apk;
  }

  public synchronized void setApk(AbsPath apk) {
    this.apk = apk;
  }

  public synchronized Optional<AbsPath> getSecondaryDexExopackageInfoDirectory() {
    return secondaryDexExopackageInfoDirectory;
  }

  public synchronized void setSecondaryDexExopackageInfoDirectory(
      Optional<AbsPath> secondaryDexExopackageInfoDirectory) {
    this.secondaryDexExopackageInfoDirectory = secondaryDexExopackageInfoDirectory;
  }

  public synchronized Optional<AbsPath> getSecondaryDexExopackageInfoMetadata() {
    return secondaryDexExopackageInfoMetadata;
  }

  public synchronized void setSecondaryDexExopackageInfoMetadata(
      Optional<AbsPath> secondaryDexExopackageInfoMetadata) {
    this.secondaryDexExopackageInfoMetadata = secondaryDexExopackageInfoMetadata;
  }

  public synchronized Optional<AbsPath> getNativeLibraryExopackageInfoDirectory() {
    return nativeLibraryExopackageInfoDirectory;
  }

  public synchronized void setNativeLibraryExopackageInfoDirectory(
      Optional<AbsPath> nativeLibraryExopackageInfoDirectory) {
    this.nativeLibraryExopackageInfoDirectory = nativeLibraryExopackageInfoDirectory;
  }

  public synchronized Optional<AbsPath> getNativeLibraryExopackageInfoMetadata() {
    return nativeLibraryExopackageInfoMetadata;
  }

  public synchronized void setNativeLibraryExopackageInfoMetadata(
      Optional<AbsPath> nativeLibraryExopackageInfoMetadata) {
    this.nativeLibraryExopackageInfoMetadata = nativeLibraryExopackageInfoMetadata;
  }

  public synchronized Optional<AbsPath> getResourcesExopackageInfoAssets() {
    return resourcesExopackageInfoAssets;
  }

  public synchronized void setResourcesExopackageInfoAssets(
      Optional<AbsPath> resourcesExopackageInfoAssets) {
    this.resourcesExopackageInfoAssets = resourcesExopackageInfoAssets;
  }

  public synchronized Optional<AbsPath> getResourcesExopackageInfoAssetsHash() {
    return resourcesExopackageInfoAssetsHash;
  }

  public synchronized void setResourcesExopackageInfoAssetsHash(
      Optional<AbsPath> resourcesExopackageInfoAssetsHash) {
    this.resourcesExopackageInfoAssetsHash = resourcesExopackageInfoAssetsHash;
  }

  public synchronized Optional<AbsPath> getResourcesExopackageInfoRes() {
    return resourcesExopackageInfoRes;
  }

  public synchronized void setResourcesExopackageInfoRes(
      Optional<AbsPath> resourcesExopackageInfoRes) {
    this.resourcesExopackageInfoRes = resourcesExopackageInfoRes;
  }

  public synchronized Optional<AbsPath> getResourcesExopackageInfoResHash() {
    return resourcesExopackageInfoResHash;
  }

  public synchronized void setResourcesExopackageInfoResHash(
      Optional<AbsPath> resourcesExopackageInfoResHash) {
    this.resourcesExopackageInfoResHash = resourcesExopackageInfoResHash;
  }
}
