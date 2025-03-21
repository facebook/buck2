/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.agent.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Collections;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * Non-instantiable class for holding functionality that runs both on the host and in the android
 * agent.
 */
public final class AgentUtil {
  private AgentUtil() {}

  // These must match the values in the agent manifest.
  public static final String AGENT_PACKAGE_NAME = "com.facebook.buck.android.agent";
  public static final String AGENT_VERSION_CODE = "15";

  /** Size in bytes of the binary data use to generate the secret key for receive-file. */
  public static final int BINARY_SECRET_KEY_SIZE = 16;

  /** Size of the text version of the receive-file secret key. */
  public static final int TEXT_SECRET_KEY_SIZE = 32;

  public static final String TEMP_PREFIX = "exopackage_temp-";

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
          sigContents = new BufferedReader(new InputStreamReader(packageZip.getInputStream(entry)));
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
