/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd.serialization.java;

import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.cd.serialization.RelPathSerializer;
import com.facebook.buck.jvm.java.JarParameters;
import com.facebook.buck.jvm.java.RemoveClassesPatternsMatcher;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableSortedSet;
import java.util.Optional;
import java.util.function.Predicate;

/** {@link JarParameters} to protobuf serializer */
public class JarParametersSerializer {

  private JarParametersSerializer() {}

  /**
   * Serializes {@link JarParameters} into javacd model's {@link
   * com.facebook.buck.cd.model.java.JarParameters}.
   */
  public static com.facebook.buck.cd.model.java.JarParameters serialize(
      JarParameters jarParameters) {
    com.facebook.buck.cd.model.java.JarParameters.Builder builder =
        com.facebook.buck.cd.model.java.JarParameters.newBuilder();

    builder.setHashEntries(jarParameters.getHashEntries());
    builder.setMergeManifests(jarParameters.getMergeManifests());
    builder.setJarPath(RelPathSerializer.serialize(jarParameters.getJarPath()));
    Predicate<Object> removeEntryPredicate = jarParameters.getRemoveEntryPredicate();
    Preconditions.checkState(removeEntryPredicate instanceof RemoveClassesPatternsMatcher);
    builder.setRemoveEntryPredicate(
        RemoveClassesPatternsMatcherSerializer.serialize(
            (RemoveClassesPatternsMatcher) removeEntryPredicate));
    for (RelPath entry : jarParameters.getEntriesToJar()) {
      builder.addEntriesToJar(RelPathSerializer.serialize(entry));
    }
    for (RelPath entry : jarParameters.getOverrideEntriesToJar()) {
      builder.addOverrideEntriesToJar(RelPathSerializer.serialize(entry));
    }

    Optional<String> mainClass = jarParameters.getMainClass();
    mainClass.ifPresent(builder::setMainClass);

    Optional<RelPath> manifestFile = jarParameters.getManifestFile();
    manifestFile.map(RelPathSerializer::serialize).ifPresent(builder::setManifestFile);

    builder.setDuplicatesLogLevel(
        LogLevelSerializer.serialize(jarParameters.getDuplicatesLogLevel()));

    return builder.build();
  }

  /**
   * Deserializes javacd model's {@link com.facebook.buck.cd.model.java.JarParameters} into {@link
   * JarParameters}.
   */
  public static JarParameters deserialize(
      com.facebook.buck.cd.model.java.JarParameters jarParameters) {
    Optional<String> mainClass =
        Optional.of(jarParameters.getMainClass()).filter(s -> !s.isEmpty());
    Optional<RelPath> manifestFile =
        Optional.of(jarParameters.getManifestFile())
            .filter(s -> !s.isEmpty())
            .map(RelPathSerializer::deserialize);

    return new JarParameters(
        jarParameters.getHashEntries(),
        jarParameters.getMergeManifests(),
        RelPathSerializer.deserialize(jarParameters.getJarPath()),
        RemoveClassesPatternsMatcherSerializer.deserialize(jarParameters.getRemoveEntryPredicate()),
        jarParameters.getEntriesToJarList().stream()
            .map(RelPathSerializer::deserialize)
            .collect(ImmutableSortedSet.toImmutableSortedSet(RelPath.comparator())),
        jarParameters.getOverrideEntriesToJarList().stream()
            .map(RelPathSerializer::deserialize)
            .collect(ImmutableSortedSet.toImmutableSortedSet(RelPath.comparator())),
        mainClass,
        manifestFile,
        LogLevelSerializer.deserialize(jarParameters.getDuplicatesLogLevel()));
  }
}
