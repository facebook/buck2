/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd.serialization.java;

import com.facebook.buck.cd.model.java.ResolvedJavacOptions;
import com.facebook.buck.jvm.java.JavacPluginParams;
import com.facebook.buck.jvm.java.ResolvedJavacPluginProperties;
import com.google.common.collect.ImmutableList;
import java.util.List;

/** {@link JavacPluginParams} to protobuf serializer */
class JavacPluginParamsSerializer {

  private JavacPluginParamsSerializer() {}

  /**
   * Serializes {@link JavacPluginParams} into javacd model's {@link
   * ResolvedJavacOptions.JavacPluginParams }.
   */
  public static ResolvedJavacOptions.JavacPluginParams serialize(
      JavacPluginParams javacPluginParams) {
    ResolvedJavacOptions.JavacPluginParams.Builder builder =
        ResolvedJavacOptions.JavacPluginParams.newBuilder();
    for (String param : javacPluginParams.getParameters()) {
      builder.addParameters(param);
    }
    for (ResolvedJavacPluginProperties pluginProperties : javacPluginParams.getPluginProperties()) {
      builder.addPluginProperties(
          ResolvedJavacPluginPropertiesSerializer.serialize(pluginProperties));
    }

    return builder.build();
  }

  /**
   * Deserializes javacd model's {@link ResolvedJavacOptions.JavacPluginParams } into {@link
   * JavacPluginParams}.
   */
  public static JavacPluginParams deserialize(
      ResolvedJavacOptions.JavacPluginParams javacPluginParams) {
    JavacPluginParams.Builder builder = JavacPluginParams.builder();
    builder.setParameters(javacPluginParams.getParametersList());
    builder.setPluginProperties(toPluginProperties(javacPluginParams.getPluginPropertiesList()));
    return builder.build();
  }

  private static Iterable<ResolvedJavacPluginProperties> toPluginProperties(
      List<ResolvedJavacOptions.ResolvedJavacPluginProperties> pluginPropertiesList) {
    ImmutableList.Builder<ResolvedJavacPluginProperties> builder =
        ImmutableList.builderWithExpectedSize(pluginPropertiesList.size());
    for (ResolvedJavacOptions.ResolvedJavacPluginProperties item : pluginPropertiesList) {
      builder.add(ResolvedJavacPluginPropertiesSerializer.deserialize(item));
    }
    return builder.build();
  }
}
