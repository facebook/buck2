/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

public class AbiDirWriter {

  public static void writeAbiOutputDir(Path existingAbi, Path outputDir) throws IOException {
    try (ZipFile zipFile = new ZipFile(existingAbi.toFile())) {
      Enumeration<? extends ZipEntry> entries = zipFile.entries();
      while (entries.hasMoreElements()) {
        ZipEntry zipEntry = entries.nextElement();
        if (!zipEntry.isDirectory()) {
          Path abiDirSubPath = outputDir.resolve(zipEntry.getName());
          Files.createDirectories(abiDirSubPath.getParent());
          Files.writeString(abiDirSubPath, Long.toHexString(zipEntry.getCrc()));
        }
      }
    }
  }
}
