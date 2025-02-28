/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.installer.apple;

import com.facebook.buck.util.json.ObjectMappers;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.type.TypeReference;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.TreeMap;

/** Apple install application options. */
class AppleInstallAppOptions {

  final String fullyQualifiedName;
  final boolean useIdb;
  // "Use this option to set the platform an apple install"
  final String platformName;
  final String xcodeDeveloperPath;
  final String deviceHelperPath;
  final Path infoPlistPath;

  /*
   *  Constructs options for an Apple Install that are set in apple_install_data.json artifact pass to the Installer
   */
  AppleInstallAppOptions(Path settingPath) throws IOException {
    JsonParser parser = ObjectMappers.createParser(settingPath);
    Map<String, String> jsonData =
        parser.readValueAs(new TypeReference<TreeMap<String, String>>() {});

    this.fullyQualifiedName = jsonData.getOrDefault("label", "");
    this.useIdb = readBoolean(jsonData, "use_idb", true);
    this.platformName = jsonData.getOrDefault("platform_name", "iphonesimulator");
    this.deviceHelperPath = jsonData.getOrDefault("device_helper_path", "");
    this.xcodeDeveloperPath = jsonData.getOrDefault("xcode_developer_path", "");
    if (xcodeDeveloperPath.isEmpty()) {
      throw new RuntimeException("xcode_developer_path must be set in apple_install_info.json");
    }

    String infoPlist = jsonData.getOrDefault("info_plist", "");
    if (infoPlist.isEmpty()) {
      throw new RuntimeException("info_plist_path must be set in apple_install_info.json");
    }
    this.infoPlistPath = Paths.get(infoPlist);
  }

  private boolean readBoolean(Map<String, String> jsonData, String name, boolean defaultValue) {
    return Boolean.parseBoolean(jsonData.getOrDefault(name, Boolean.toString(defaultValue)));
  }
}
