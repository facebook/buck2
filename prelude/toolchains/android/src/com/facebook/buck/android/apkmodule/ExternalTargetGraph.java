/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.apkmodule;

import com.google.common.collect.ImmutableMap;
import java.util.Set;

/**
 * A "target graph" that is constructed based on an external graph, that can then be used to create
 * an {@link APKModuleGraph}
 */
class ExternalTargetGraph implements TargetGraphInterface<ExternalTargetGraph.ExternalBuildTarget> {
  private final ImmutableMap<ExternalBuildTarget, ExternalTargetNode> map;
  private final ImmutableMap<String, ExternalBuildTarget> nameToBuildTargetMap;

  public ExternalTargetGraph(
      ImmutableMap<ExternalBuildTarget, ExternalTargetNode> map,
      ImmutableMap<String, ExternalBuildTarget> nameToBuildTargetMap) {
    this.map = map;
    this.nameToBuildTargetMap = nameToBuildTargetMap;
  }

  ExternalBuildTarget getBuildTarget(String buildTargetName) {
    return nameToBuildTargetMap.get(buildTargetName);
  }

  @Override
  public boolean isEmpty() {
    return false;
  }

  @Override
  public HasBuildTargetAndBuildDeps<ExternalBuildTarget> get(ExternalBuildTarget buildTarget) {
    return map.get(buildTarget);
  }

  /** A node in an external target graph. */
  static class ExternalTargetNode implements HasBuildTargetAndBuildDeps<ExternalBuildTarget> {
    private final ExternalBuildTarget buildTarget;
    private final Set<ExternalBuildTarget> buildDeps;

    public ExternalTargetNode(ExternalBuildTarget buildTarget) {
      this(buildTarget, Set.of());
    }

    public ExternalTargetNode(ExternalBuildTarget buildTarget, Set<ExternalBuildTarget> buildDeps) {
      this.buildTarget = buildTarget;
      this.buildDeps = buildDeps;
    }

    @Override
    public ExternalBuildTarget getBuildTarget() {
      return buildTarget;
    }

    @Override
    public Set<ExternalBuildTarget> getBuildDeps() {
      return buildDeps;
    }
  }

  /** A representation of a build target that just uses the build target's name */
  static class ExternalBuildTarget implements Comparable<ExternalBuildTarget> {
    private final String name;

    public ExternalBuildTarget(String name) {
      this.name = name;
    }

    @Override
    public int compareTo(ExternalBuildTarget o) {
      return this.name.compareTo(o.name);
    }

    String getName() {
      return name;
    }

    @Override
    public String toString() {
      return getClass().getSimpleName() + ":" + Integer.toHexString(hashCode()) + "(" + name + ")";
    }
  }
}
