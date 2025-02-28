/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.core.util.immutables.BuckStyleValueWithBuilder;
import com.google.common.collect.ImmutableSortedSet;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.logging.Level;
import org.immutables.value.Value;

@BuckStyleValueWithBuilder
public abstract class JarParameters {
  @Value.Default
  public boolean getHashEntries() {
    return false;
  }

  @Value.Default
  public boolean getMergeManifests() {
    return false;
  }

  public abstract RelPath getJarPath();

  @Value.Default
  public Predicate<Object> getRemoveEntryPredicate() {
    return RemoveClassesPatternsMatcher.EMPTY;
  }

  public abstract ImmutableSortedSet<RelPath> getEntriesToJar();

  @Value.Default
  public ImmutableSortedSet<RelPath> getOverrideEntriesToJar() {
    return ImmutableSortedSet.of();
  }

  public abstract Optional<String> getMainClass();

  public abstract Optional<RelPath> getManifestFile();

  @Value.Default
  public Level getDuplicatesLogLevel() {
    return Level.INFO;
  }

  public static Builder builder() {
    return new Builder();
  }

  public static class Builder extends ImmutableJarParameters.Builder {}
}
