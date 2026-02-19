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

import com.facebook.infer.annotation.Nullsafe;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.Multimap;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

@Nullsafe(Nullsafe.Mode.LOCAL)
public class ExopackageUtil {
  public static ImmutableMap<Path, Path> applyFilenameFormat(
      Map<String, Path> filesToHashes, Path deviceDir, String filenameFormat) {
    ImmutableMap.Builder<Path, Path> filesBuilder = ImmutableMap.builder();
    for (Map.Entry<String, Path> entry : filesToHashes.entrySet()) {
      filesBuilder.put(
          deviceDir.resolve(String.format(filenameFormat, entry.getKey())), entry.getValue());
    }
    return filesBuilder.build();
  }

  public static ImmutableMultimap<Path, Path> applyFilenameFormat(
      Multimap<String, Path> filesToHashes, Path deviceDir, String filenameFormat) {
    ImmutableMultimap.Builder<Path, Path> filesBuilder = ImmutableMultimap.builder();
    for (Map.Entry<String, Path> entry : filesToHashes.entries()) {
      filesBuilder.put(
          deviceDir.resolve(String.format(filenameFormat, entry.getKey())), entry.getValue());
    }
    return filesBuilder.build();
  }

  public static String getJarSignature(String packagePath) throws IOException {
    Pattern signatureFilePattern = Pattern.compile("META-INF/[A-Z]+\\.SF");

    ZipFile packageZip = null;
    try {
      packageZip = new ZipFile(packagePath);
      // For each file in the zip.
      for (ZipEntry entry : Collections.list(packageZip.entries())) {
        // Ignore non-signature files.
        if (!signatureFilePattern.matcher(entry.getName()).matches()) {
          continue;
        }

        BufferedReader sigContents = null;
        try {
          sigContents =
              new BufferedReader(
                  new InputStreamReader(Objects.requireNonNull(packageZip.getInputStream(entry))));
          // For each line in the signature file.
          while (true) {
            String line = sigContents.readLine();
            if (line == null || line.equals("")) {
              throw new IllegalArgumentException(
                  "Failed to find manifest digest in " + entry.getName());
            }
            String prefix_sha1 = "SHA1-Digest-Manifest: ";
            String prefix_sha256 = "SHA-256-Digest-Manifest: ";
            if (line.startsWith(prefix_sha1)) {
              return line.substring(prefix_sha1.length());
            } else if (line.startsWith(prefix_sha256)) {
              return line.substring(prefix_sha256.length());
            }
          }
        } finally {
          if (sigContents != null) {
            sigContents.close();
          }
        }
      }
    } finally {
      if (packageZip != null) {
        packageZip.close();
      }
    }

    throw new IllegalArgumentException("Failed to find signature file.");
  }
}
