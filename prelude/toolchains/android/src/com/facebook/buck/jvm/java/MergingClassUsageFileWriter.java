/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.core.exceptions.HumanReadableException;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.facebook.buck.jvm.kotlin.compilerplugins.usedclasses.ClassUsageMerger;
import com.facebook.buck.util.json.ObjectMappers;
import com.fasterxml.jackson.core.type.TypeReference;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableMap;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public final class MergingClassUsageFileWriter extends DefaultClassUsageFileWriter {

  private final AbsPath prevClassUsageFile;

  public MergingClassUsageFileWriter(AbsPath prevClassUsageFile) {
    this.prevClassUsageFile = prevClassUsageFile;
  }

  @Override
  public void writeFile(
      ImmutableMap<Path, Set<Path>> classUsages,
      RelPath relativePath,
      AbsPath rootPath,
      RelPath configuredBuckOut) {
    try {
      AbsPath parent = rootPath.resolve(relativePath).getParent();
      Path parentRelativePath = rootPath.relativize(parent).getPath();
      Preconditions.checkState(
          ProjectFilesystemUtils.exists(rootPath, parentRelativePath),
          "directory must exist: %s",
          parent);

      ObjectMappers.WRITER.writeValue(
          rootPath.resolve(relativePath).toFile(),
          ClassUsageMerger.mergeClassUsageMaps(
              createClassUsageMap(prevClassUsageFile.getPath()),
              relativizeMap(classUsages, rootPath, configuredBuckOut)));
    } catch (IOException e) {
      throw new HumanReadableException(e, "Unable to write used classes file.");
    }
  }

  private static Map<Path, Set<Path>> createClassUsageMap(Path jsonPath) throws IOException {
    if (!Files.exists(jsonPath)) {
      return new HashMap<>();
    }

    return ObjectMappers.readValue(jsonPath, new TypeReference<Map<Path, Set<Path>>>() {});
  }
}
