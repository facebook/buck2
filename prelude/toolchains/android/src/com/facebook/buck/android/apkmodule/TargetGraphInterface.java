/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.apkmodule;

/**
 * Interface for a directed graph of targets that can be traversed by getting the dependencies of
 * each node.
 */
public interface TargetGraphInterface<BuildTarget> {
  boolean isEmpty();

  HasBuildTargetAndBuildDeps<BuildTarget> get(BuildTarget buildTarget);
}
