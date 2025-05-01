/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.google.common.collect.ImmutableMap;
import java.nio.file.Path;
import java.util.Set;

public interface ClassUsageFileWriter {

  void writeFile(
      ImmutableMap<Path, Set<Path>> classUsages,
      RelPath relativePath,
      AbsPath rootPath,
      RelPath configuredBuckOut);
}
