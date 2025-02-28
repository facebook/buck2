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
import com.facebook.buck.core.util.immutables.BuckStyleValue;
import com.facebook.buck.core.util.immutables.BuckStyleValueWithBuilder;
import com.google.common.collect.ImmutableList;
import java.util.Optional;

/**
 * Isolated ExopackageInfo. Doesn't have any references to SourcePath and other buck's internal data
 * structures.
 */
@BuckStyleValueWithBuilder
public abstract class IsolatedExopackageInfo {

  /** Isolated DexInfo */
  @BuckStyleValue
  public interface IsolatedDexInfo {
    AbsPath getMetadata();

    AbsPath getDirectory();

    static IsolatedExopackageInfo.IsolatedDexInfo of(AbsPath metadata, AbsPath directory) {
      return ImmutableIsolatedDexInfo.ofImpl(metadata, directory);
    }
  }

  /** Isolated NativeLibsInfo */
  @BuckStyleValue
  public interface IsolatedNativeLibsInfo {
    AbsPath getMetadata();

    AbsPath getDirectory();

    static IsolatedExopackageInfo.IsolatedNativeLibsInfo of(AbsPath metadata, AbsPath directory) {
      return ImmutableIsolatedNativeLibsInfo.ofImpl(metadata, directory);
    }
  }

  /** Isolated ResourcesInfo */
  @BuckStyleValue
  public interface IsolatedResourcesInfo {
    ImmutableList<IsolatedExopackagePathAndHash> getResourcesPaths();

    static IsolatedExopackageInfo.IsolatedResourcesInfo of(
        ImmutableList<IsolatedExopackagePathAndHash> resourcesPaths) {
      return ImmutableIsolatedResourcesInfo.ofImpl(resourcesPaths);
    }
  }

  /** Isolated ExopackagePathAndHash */
  @BuckStyleValue
  public interface IsolatedExopackagePathAndHash {

    static IsolatedExopackagePathAndHash of(AbsPath path, AbsPath hashPath) {
      return ImmutableIsolatedExopackagePathAndHash.ofImpl(path, hashPath);
    }

    AbsPath getPath();

    AbsPath getHashPath();
  }

  public abstract Optional<IsolatedExopackageInfo.IsolatedDexInfo> getDexInfo();

  public abstract Optional<IsolatedExopackageInfo.IsolatedNativeLibsInfo> getNativeLibsInfo();

  public abstract Optional<IsolatedExopackageInfo.IsolatedResourcesInfo> getResourcesInfo();

  public static IsolatedExopackageInfo.Builder builder() {
    return new IsolatedExopackageInfo.Builder();
  }

  public static class Builder extends ImmutableIsolatedExopackageInfo.Builder {}
}
