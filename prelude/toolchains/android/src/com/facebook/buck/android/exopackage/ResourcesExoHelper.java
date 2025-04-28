/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.exopackage;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.io.filesystem.impl.ProjectFilesystemUtils;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableMap;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Stream;

/** Installs resources for exo. */
public class ResourcesExoHelper implements ExoHelper {

  @VisibleForTesting public static final Path RESOURCES_DIR = Paths.get("resources");

  private final AbsPath rootPath;
  private final IsolatedExopackageInfo.IsolatedResourcesInfo resourcesInfo;

  ResourcesExoHelper(AbsPath rootPath, IsolatedExopackageInfo.IsolatedResourcesInfo resourcesInfo) {
    this.rootPath = rootPath;
    this.resourcesInfo = resourcesInfo;
  }

  private static ImmutableMap<Path, Path> getFilesToInstall(
      ImmutableMap<String, Path> filesByHash) {
    return ExopackageUtil.applyFilenameFormat(filesByHash, RESOURCES_DIR, "%s.apk");
  }

  /** Returns a map of hash to path for resource files. */
  private static ImmutableMap<String, Path> getResourceFilesByHash(
      AbsPath rootPath,
      Stream<IsolatedExopackageInfo.IsolatedExopackagePathAndHash> resourcesPaths) {
    return resourcesPaths
        .filter(
            pathAndHash ->
                ProjectFilesystemUtils.exists(rootPath, pathAndHash.getHashPath().getPath()))
        .collect(
            ImmutableMap.toImmutableMap(
                pathAndHash ->
                    ProjectFilesystemUtils.readFileIfItExists(
                            rootPath, pathAndHash.getHashPath().getPath())
                        .get()
                        .stripTrailing(),
                i -> i.getPath().getPath()));
  }

  @Override
  public String getType() {
    return "resources";
  }

  @Override
  public ImmutableMap<Path, Path> getFilesToInstall() {
    return getFilesToInstall(getResourceFilesByHash());
  }

  @Override
  public ImmutableMap<Path, String> getMetadataToInstall() {
    return ImmutableMap.of(
        RESOURCES_DIR.resolve("metadata.txt"),
        getResourceMetadataContents(getResourceFilesByHash()));
  }

  private ImmutableMap<String, Path> getResourceFilesByHash() {
    return getResourceFilesByHash(rootPath, resourcesInfo.getResourcesPaths().stream());
  }

  private static String getResourceMetadataContents(ImmutableMap<String, Path> filesByHash) {
    return Joiner.on("\n")
        .join(filesByHash.keySet().stream().map(h -> "resources " + h).iterator());
  }
}
