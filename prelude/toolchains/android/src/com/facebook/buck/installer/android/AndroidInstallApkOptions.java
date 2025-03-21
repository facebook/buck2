/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.installer.android;

import com.facebook.buck.util.environment.EnvVariablesProvider;
import com.facebook.buck.util.environment.Platform;
import com.facebook.buck.util.json.ObjectMappers;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.type.TypeReference;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

/**
 * Constructs configurations for an android install that are set in install_android_options.json and
 * sent to the AndroidInstaller
 */
public class AndroidInstallApkOptions {

  public final String adbExecutable;
  public final boolean restartAdbOnFailure;
  public final boolean stagedInstallMode;
  public final boolean skipInstallMetadata;
  public final boolean isZstdCompressionEnabled;
  public final int agentPortBase;
  public final int adbMaxRetries;
  public final long adbRetryDelayMs;
  public final boolean apexMode;

  AndroidInstallApkOptions(Path jsonArtifactPath) throws RuntimeException, IOException {
    JsonParser parser = ObjectMappers.createParser(jsonArtifactPath);
    Map<String, String> jsonData =
        parser.readValueAs(new TypeReference<TreeMap<String, String>>() {});
    this.adbExecutable = getAdbExecutable(jsonData);
    this.restartAdbOnFailure = readBoolean(jsonData, "adb_restart_on_failure");
    this.stagedInstallMode = readBoolean(jsonData, "staged_install_mode");
    this.apexMode = readBoolean(jsonData, "apex_mode");
    this.skipInstallMetadata = readBoolean(jsonData, "skip_install_metadata");
    this.isZstdCompressionEnabled = readBoolean(jsonData, "is_zstd_compression_enabled");
    this.agentPortBase = readInt(jsonData, "agent_port_base", 2828);
    this.adbMaxRetries = readInt(jsonData, "max_retries", 5);
    this.adbRetryDelayMs = readInt(jsonData, "retry_delay_millis", 500);
  }

  private boolean readBoolean(Map<String, String> jsonData, String name) {
    return Boolean.parseBoolean(jsonData.getOrDefault(name, Boolean.FALSE.toString()));
  }

  private int readInt(Map<String, String> jsonData, String name, int defaultValue) {
    String readValue = jsonData.getOrDefault(name, "");
    if (readValue.isEmpty()) {
      return defaultValue;
    }
    return Integer.parseInt(readValue);
  }

  private String getAdbExecutable(Map<String, String> jsonData) {
    String adbExecutableFromJsonArtifact = jsonData.get("adb_executable");
    if (adbExecutableFromJsonArtifact != null) {
      return adbExecutableFromJsonArtifact;
    }

    return getAdbFromPath().orElse("/opt/android_sdk/platform-tools/adb");
  }

  private Optional<String> getAdbFromPath() {
    String path = EnvVariablesProvider.getSystemEnv().get("PATH");
    if (path != null) {
      String binaryExtension = Platform.detect() == Platform.WINDOWS ? ".exe" : "";
      String adbBinary = "adb" + binaryExtension;
      for (String pathEntry : path.split(File.pathSeparator)) {
        Path entry = Paths.get(pathEntry);
        Path adb = entry.resolve(adbBinary);
        if (adb.toFile().exists()) {
          return Optional.of(adb.toString());
        }
      }
    }

    return Optional.empty();
  }
}
