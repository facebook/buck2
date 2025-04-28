/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.apk;

import com.facebook.buck.android.apk.sdk.ApkBuilder;
import com.facebook.buck.android.apk.sdk.ApkCreationException;
import com.facebook.buck.android.apk.sdk.DuplicateFileException;
import com.facebook.buck.android.apk.sdk.SealedApkException;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;

/** A class that provides methods useful for building an APK. */
public class ApkBuilderUtils {

  /**
   * The type of a keystore created via the {@code jarsigner} command in Sun/Oracle Java. See
   * http://docs.oracle.com/javase/7/docs/technotes/guides/security/StandardNames.html#KeyStore.
   */
  private static final String JARSIGNER_KEY_STORE_TYPE = "jks";

  public static void buildApk(
      Path resourceApk,
      Path pathToOutputApkFile,
      Path dexFile,
      ImmutableSet<Path> assetDirectories,
      ImmutableSet<Path> nativeLibraryDirectories,
      ImmutableSet<Path> zipFiles,
      ImmutableSet<Path> jarFilesThatMayContainResources,
      Path pathToKeystore,
      KeystoreProperties keystoreProperties,
      boolean packageMetaInfVersionFiles,
      PrintStream output,
      ImmutableSet<String> excludedResources)
      throws IOException,
          ApkCreationException,
          KeyStoreException,
          NoSuchAlgorithmException,
          SealedApkException,
          UnrecoverableKeyException,
          DuplicateFileException {
    ApkBuilder builder =
        new ApkBuilder(
            pathToOutputApkFile.toFile(),
            resourceApk.toFile(),
            dexFile.toFile(),
            packageMetaInfVersionFiles,
            output,
            excludedResources);
    for (Path nativeLibraryDirectory : nativeLibraryDirectories) {
      builder.addNativeLibraries(nativeLibraryDirectory.toFile());
    }
    for (Path assetDirectory : assetDirectories) {
      builder.addSourceFolder(assetDirectory.toFile());
    }
    for (Path zipFile : zipFiles) {
      // TODO(natthu): Skipping silently is bad. These should really be assertions.
      if (Files.exists(zipFile) && Files.isRegularFile(zipFile)) {
        builder.addZipFile(zipFile.toFile());
      }
    }
    for (Path jarFileThatMayContainResources : jarFilesThatMayContainResources) {
      builder.addResourcesFromJar(jarFileThatMayContainResources.toFile());
    }

    // Build the APK
    builder.sealApk();
  }
}
