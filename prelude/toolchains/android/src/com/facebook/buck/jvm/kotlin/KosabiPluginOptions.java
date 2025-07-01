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
import com.google.common.collect.ImmutableMap;
import java.util.Map;

public class KosabiPluginOptions {
  private final ImmutableMap<String, AbsPath> resolvedKosabiPluginOptionPath;
  private final boolean shouldUseStandaloneKosabi;

  public KosabiPluginOptions(
      ImmutableMap<String, AbsPath> resolvedKosabiPluginOptionPath,
      boolean shouldUseStandaloneKosabi) {
    this.resolvedKosabiPluginOptionPath = resolvedKosabiPluginOptionPath;
    this.shouldUseStandaloneKosabi = shouldUseStandaloneKosabi;
  }

  public ImmutableMap<String, AbsPath> getAllKosabiPlugins() {
    return resolvedKosabiPluginOptionPath;
  }

  private ImmutableMap<String, AbsPath> getAllKosabiPluginsExceptStubgen() {
    return resolvedKosabiPluginOptionPath.entrySet().stream()
        .filter(entry -> !KosabiConfig.PROPERTY_KOSABI_STUBS_GEN_PLUGIN.equals(entry.getKey()))
        .collect(ImmutableMap.toImmutableMap(Map.Entry::getKey, Map.Entry::getValue));
  }

  public ImmutableMap<String, AbsPath> getKosabiPlugins() {
    return shouldUseStandaloneKosabi ? getAllKosabiPluginsExceptStubgen() : getAllKosabiPlugins();
  }
}
