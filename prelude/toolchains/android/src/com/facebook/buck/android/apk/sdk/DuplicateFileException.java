/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.apk.sdk;

import com.facebook.buck.android.apk.sdk.ApkJarBuilder.IZipEntryFilter.ZipAbortException;
import java.io.File;

/** An exception thrown during packaging of an APK file. */
public final class DuplicateFileException extends ZipAbortException {
  private static final long serialVersionUID = 1L;
  private final String mArchivePath;
  private final File mFile1;
  private final File mFile2;

  public DuplicateFileException(String archivePath, File file1, File file2) {
    super();
    mArchivePath = archivePath;
    mFile1 = file1;
    mFile2 = file2;
  }

  public String getArchivePath() {
    return mArchivePath;
  }

  public File getFile1() {
    return mFile1;
  }

  public File getFile2() {
    return mFile2;
  }

  @Override
  public String getMessage() {
    return "Duplicate files at the same path inside the APK";
  }
}
