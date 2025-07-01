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

import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.cd.serialization.RelPathSerializer;
import com.facebook.buck.jvm.java.ResolvedJavacOptions;
import com.google.common.collect.ImmutableList;
import java.util.Optional;

/** {@link ResolvedJavacOptions} to protobuf serializer */
public class ResolvedJavacOptionsSerializer {

  private ResolvedJavacOptionsSerializer() {}

  /**
   * Serializes {@link ResolvedJavacOptions} into javacd model's {@link
   * com.facebook.buck.cd.model.java.ResolvedJavacOptions}.
   */
  public static com.facebook.buck.cd.model.java.ResolvedJavacOptions serialize(
      ResolvedJavacOptions options) {
    var builder = com.facebook.buck.cd.model.java.ResolvedJavacOptions.newBuilder();

    Optional<String> bootclasspath = options.getBootclasspath();
    bootclasspath.ifPresent(builder::setBootclasspath);
    builder.setDebug(options.getDebug());
    builder.setVerbose(options.getVerbose());

    ImmutableList<RelPath> bootclasspathList = options.getBootclasspathList();
    bootclasspathList.stream()
        .map(RelPathSerializer::serialize)
        .forEach(builder::addBootclasspathList);

    builder.setLanguageLevelOptions(
        JavacLanguageLevelOptionsSerializer.serialize(options.getLanguageLevelOptions()));

    builder.setJavaAnnotationProcessorParams(
        JavacPluginParamsSerializer.serialize(options.getJavaAnnotationProcessorParams()));
    builder.setStandardJavacPluginParams(
        JavacPluginParamsSerializer.serialize(options.getStandardJavacPluginParams()));

    for (String extraArg : options.getExtraArguments()) {
      builder.addExtraArguments(extraArg);
    }

    return builder.build();
  }

  /**
   * Deserializes javacd model's {@link com.facebook.buck.cd.model.java.ResolvedJavacOptions} into
   * {@link ResolvedJavacOptions}.
   */
  public static ResolvedJavacOptions deserialize(
      com.facebook.buck.cd.model.java.ResolvedJavacOptions options) {
    var bootclasspathListList = options.getBootclasspathListList();
    ImmutableList<RelPath> bootclasspathList =
        bootclasspathListList.stream()
            .map(RelPathSerializer::deserialize)
            .collect(ImmutableList.toImmutableList());

    return new ResolvedJavacOptions(
        toOptionalString(options.getBootclasspath()),
        bootclasspathList,
        JavacLanguageLevelOptionsSerializer.deserialize(options.getLanguageLevelOptions()),
        options.getDebug(),
        options.getVerbose(),
        JavacPluginParamsSerializer.deserialize(options.getJavaAnnotationProcessorParams()),
        JavacPluginParamsSerializer.deserialize(options.getStandardJavacPluginParams()),
        ImmutableList.copyOf(options.getExtraArgumentsList()),
        options.getSystemImage().isEmpty() ? null : options.getSystemImage());
  }

  private static Optional<String> toOptionalString(String value) {
    return value.isEmpty() ? Optional.empty() : Optional.of(value);
  }
}
