/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.testrunner.reportlayer;

import com.android.ddmlib.IDevice;
import com.facebook.buck.testrunner.InstrumentationTestRunner;
import java.net.URI;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.HashMap;
import java.util.Iterator;

/** report layer to collect tombstones to TRA after tests finish */
public class TombstonesReportLayer extends ReportLayer {

  private static final String TOMBSTONE_REMOTE_PATH = "/data/tombstones/";

  public static final String ARG = "--collect-tombstones";

  public TombstonesReportLayer(InstrumentationTestRunner runner) {
    super(runner);
  }

  @Override
  public void initialize() {}

  @Override
  public void report() {
    try {
      this.collectTombstones(this.runner.getDevice());
    } catch (Exception e) {
      System.err.printf("Failed to collect tombstones with error: %s\n", e);
    }
  }

  private void collectTombstones(IDevice device) throws Exception {
    if (!this.runner.directoryExists(TOMBSTONE_REMOTE_PATH, device)) {
      return;
    }
    // get the tombstones from the device
    Path tmp = Files.createTempDirectory("ait-tombstones-");
    if (!Files.exists(tmp)) {
      Files.createDirectory(tmp);
    }
    this.runner.pullDir(device, TOMBSTONE_REMOTE_PATH, tmp.toString());

    // check whether the dir is empty
    if (Files.list(tmp).count() == 0) {
      return;
    }

    Path artifact_path =
        this.runner.createTRA("generic_blob", "Zip file with all tombstones", "tombstones.zip");
    if (null == artifact_path) {
      // we cannot report to TRA, because the dir is not known. So skip it.
      return;
    }

    // pack them into a zip
    URI uri = URI.create("jar:" + artifact_path.toUri());
    HashMap<String, String> env = new HashMap<>();
    env.put("create", "true");
    try (FileSystem zipfs = FileSystems.newFileSystem(uri, env)) {
      Iterator<Path> tombstones = Files.walk(tmp).filter(Files::isRegularFile).iterator();
      while (tombstones.hasNext()) {
        Path path = tombstones.next();
        Path target = zipfs.getPath(tmp.relativize(path).toString());
        if (null != target.getParent()) {
          Files.createDirectory(target.getParent());
        }
        Files.copy(path, target, StandardCopyOption.REPLACE_EXISTING);
      }
    }
  }
}
