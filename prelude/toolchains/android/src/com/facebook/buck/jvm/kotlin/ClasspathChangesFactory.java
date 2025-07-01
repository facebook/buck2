/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.core.util.log.Logger;
import com.facebook.buck.io.file.FileExtensionMatcher;
import com.facebook.buck.io.file.PathMatcher;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.ClasspathChanges;
import com.google.common.collect.ImmutableList;
import java.nio.file.Path;
import java.util.Map;
import java.util.stream.Collectors;

public class ClasspathChangesFactory {

  private static final Logger LOG = Logger.get(ClasspathChangesFactory.class);
  private static final PathMatcher JAR_PATH_MATCHER = FileExtensionMatcher.of("jar");

  private ClasspathChangesFactory() {}

  public static ClasspathChanges create(
      ActionMetadata actionMetadata, ImmutableList<AbsPath> classpathSnapshots) {
    Map<Path, String> currentJars = filterJarFiles(actionMetadata.getCurrentDigest());
    Map<Path, String> previousJars = filterJarFiles(actionMetadata.getPreviousDigest());

    if (currentJars.size() != previousJars.size()) {
      LOG.info("Classpath changes: Detected additions/removals on the classpath");
      return new ClasspathChanges.ToBeComputedByIncrementalCompiler(
          ImmutableList.copyOf(
              classpathSnapshots.stream().map(AbsPath::toFile).collect(Collectors.toList())));
    }

    if (!currentJars.equals(previousJars)) {
      LOG.info("Classpath changes: Detected changes on the classpath");
      return new ClasspathChanges.ToBeComputedByIncrementalCompiler(
          ImmutableList.copyOf(
              classpathSnapshots.stream().map(AbsPath::toFile).collect(Collectors.toList())));
    }

    LOG.info("Classpath changes: No changes on the classpath");
    return new ClasspathChanges.NoChanges(
        ImmutableList.copyOf(
            classpathSnapshots.stream().map(AbsPath::toFile).collect(Collectors.toList())));
  }

  private static Map<Path, String> filterJarFiles(Map<Path, String> digest) {
    return digest.entrySet().stream()
        .filter(pathStringEntry -> JAR_PATH_MATCHER.matches(RelPath.of(pathStringEntry.getKey())))
        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
  }
}
