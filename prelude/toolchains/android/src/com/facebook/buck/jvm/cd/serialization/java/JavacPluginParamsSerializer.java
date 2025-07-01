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
import com.facebook.buck.jvm.java.JavacPluginParams;
import com.facebook.buck.jvm.java.ResolvedJavacPluginProperties;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSortedSet;
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
    return new JavacPluginParams(
        toPluginProperties(javacPluginParams.getPluginPropertiesList()),
        ImmutableSortedSet.copyOf(javacPluginParams.getParametersList()));
  }

  private static ImmutableList<ResolvedJavacPluginProperties> toPluginProperties(
      List<ResolvedJavacOptions.ResolvedJavacPluginProperties> pluginPropertiesList) {
    ImmutableList.Builder<ResolvedJavacPluginProperties> builder =
        ImmutableList.builderWithExpectedSize(pluginPropertiesList.size());
    for (ResolvedJavacOptions.ResolvedJavacPluginProperties item : pluginPropertiesList) {
      builder.add(ResolvedJavacPluginPropertiesSerializer.deserialize(item));
    }
    return builder.build();
  }
}
