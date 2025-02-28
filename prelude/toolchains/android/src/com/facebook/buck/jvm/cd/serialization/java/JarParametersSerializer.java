/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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
    JarParameters.Builder builder = JarParameters.builder();

    builder.setHashEntries(jarParameters.getHashEntries());
    builder.setMergeManifests(jarParameters.getMergeManifests());
    builder.setJarPath(RelPathSerializer.deserialize(jarParameters.getJarPath()));
    builder.setRemoveEntryPredicate(
        RemoveClassesPatternsMatcherSerializer.deserialize(
            jarParameters.getRemoveEntryPredicate()));
    builder.setEntriesToJar(
        jarParameters.getEntriesToJarList().stream()
            .map(RelPathSerializer::deserialize)
            .collect(ImmutableSortedSet.toImmutableSortedSet(RelPath.comparator())));
    builder.setOverrideEntriesToJar(
        jarParameters.getOverrideEntriesToJarList().stream()
            .map(RelPathSerializer::deserialize)
            .collect(ImmutableSortedSet.toImmutableSortedSet(RelPath.comparator())));

    String mainClass = jarParameters.getMainClass();
    if (!mainClass.isEmpty()) {
      builder.setMainClass(mainClass);
    }
    builder.setManifestFile(
        Optional.of(jarParameters.getManifestFile())
            .filter(s -> !s.isEmpty())
            .map(RelPathSerializer::deserialize));

    builder.setDuplicatesLogLevel(
        LogLevelSerializer.deserialize(jarParameters.getDuplicatesLogLevel()));

    return builder.build();
  }
}
