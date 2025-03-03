/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.core;

import com.facebook.buck.cd.model.java.BuildTargetValue.Type;
import com.facebook.buck.core.util.immutables.BuckStyleValue;
import org.immutables.value.Value;

/**
 * Build target representation used in java compilation. This class include only fields that used in
 * java compilation.
 */
@BuckStyleValue
public abstract class BuildTargetValue {

  public abstract Type getType();

  public abstract String getFullyQualifiedName();

  @Value.Derived
  public boolean hasAbiJar() {
    return isSourceAbi() || isSourceOnlyAbi();
  }

  @Value.Derived
  public boolean isLibraryJar() {
    return getType() == Type.LIBRARY;
  }

  @Value.Derived
  public boolean isSourceAbi() {
    return getType() == Type.SOURCE_ABI;
  }

  @Value.Derived
  public boolean isSourceOnlyAbi() {
    return getType() == Type.SOURCE_ONLY_ABI;
  }

  @Override
  public String toString() {
    return getFullyQualifiedName();
  }

  /** Creates {@link BuildTargetValue} */
  public static BuildTargetValue of(Type type, String fullyQualifiedName) {
    return ImmutableBuildTargetValue.ofImpl(type, fullyQualifiedName);
  }
}
