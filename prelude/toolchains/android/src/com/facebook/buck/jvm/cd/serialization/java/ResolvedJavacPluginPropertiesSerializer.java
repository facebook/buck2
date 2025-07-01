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

import com.facebook.buck.cd.model.java.ResolvedJavacOptions;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.cd.serialization.RelPathSerializer;
import com.facebook.buck.jvm.java.ResolvedJavacPluginProperties;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;
import java.util.Map;

/** {@link ResolvedJavacPluginProperties} to protobuf serializer */
class ResolvedJavacPluginPropertiesSerializer {

  private ResolvedJavacPluginPropertiesSerializer() {}

  /**
   * Serializes {@link ResolvedJavacPluginProperties} into javacd model's {@link
   * ResolvedJavacOptions.ResolvedJavacPluginProperties}.
   */
  public static ResolvedJavacOptions.ResolvedJavacPluginProperties serialize(
      ResolvedJavacPluginProperties pluginProperties) {
    ResolvedJavacOptions.ResolvedJavacPluginProperties.Builder builder =
        ResolvedJavacOptions.ResolvedJavacPluginProperties.newBuilder();

    builder.setCanReuseClassLoader(pluginProperties.getCanReuseClassLoader());
    builder.setDoesNotAffectAbi(pluginProperties.getDoesNotAffectAbi());
    builder.setSupportsAbiGenerationFromSource(
        pluginProperties.getSupportAbiGenerationFromSource());
    builder.setRunsOnJavaOnly(pluginProperties.getRunsOnJavaOnly());
    for (String processorName : pluginProperties.getProcessorNames()) {
      builder.addProcessorNames(processorName);
    }
    for (RelPath classpath : pluginProperties.getClasspath()) {
      builder.addClasspath(RelPathSerializer.serialize(classpath));
    }

    pluginProperties
        .getPathParams()
        .forEach((key, value) -> builder.putPathParams(key, RelPathSerializer.serialize(value)));

    return builder.build();
  }

  /**
   * Deserializes javacd model's {@link ResolvedJavacOptions.ResolvedJavacPluginProperties} into
   * {@link ResolvedJavacPluginProperties}.
   */
  public static ResolvedJavacPluginProperties deserialize(
      ResolvedJavacOptions.ResolvedJavacPluginProperties pluginProperties) {
    return new ResolvedJavacPluginProperties(
        pluginProperties.getCanReuseClassLoader(),
        pluginProperties.getDoesNotAffectAbi(),
        pluginProperties.getSupportsAbiGenerationFromSource(),
        pluginProperties.getRunsOnJavaOnly(),
        ImmutableSortedSet.copyOf(pluginProperties.getProcessorNamesList()),
        pluginProperties.getClasspathList().stream()
            .map(RelPathSerializer::deserialize)
            .collect(ImmutableList.toImmutableList()),
        toPathParams(pluginProperties.getPathParamsMap()),
        ImmutableList.copyOf(pluginProperties.getArgumentsList()));
  }

  private static ImmutableMap<String, RelPath> toPathParams(Map<String, String> pathParamsMap) {
    ImmutableMap.Builder<String, RelPath> pathParamsBuilder =
        ImmutableMap.builderWithExpectedSize(pathParamsMap.size());
    for (Map.Entry<String, String> entry : pathParamsMap.entrySet()) {
      pathParamsBuilder.put(entry.getKey(), RelPathSerializer.deserialize(entry.getValue()));
    }
    return pathParamsBuilder.build();
  }
}
