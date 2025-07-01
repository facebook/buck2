/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd.serialization.kotlin;

import com.facebook.buck.cd.model.kotlin.PluginParams;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams;
import com.facebook.buck.jvm.cd.serialization.AbsPathSerializer;
import com.facebook.buck.jvm.cd.serialization.java.ResolvedJavacOptionsSerializer;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;
import java.util.Map;
import java.util.Optional;

/**
 * Marshalling between:
 *
 * <ul>
 *   <li>{@link com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams} (the parameters needed to
 *       create the steps for compiling Kotlin libraries), and
 *   <li>{@link com.facebook.buck.cd.model.kotlin.KotlinExtraParams} (part of the protocol buffer
 *       model).
 * </ul>
 */
public class KotlinExtraParamsSerializer {

  private KotlinExtraParamsSerializer() {}

  /** Internal buck representation to protocol buffer model */
  public static com.facebook.buck.cd.model.kotlin.KotlinExtraParams serialize(
      KotlinExtraParams kotlinExtraParams) {
    com.facebook.buck.cd.model.kotlin.KotlinExtraParams.Builder builder =
        com.facebook.buck.cd.model.kotlin.KotlinExtraParams.newBuilder();

    builder.setStandardLibraryClassPath(
        AbsPathSerializer.serialize(kotlinExtraParams.getStandardLibraryClassPath()));
    builder.setAnnotationProcessingClassPath(
        AbsPathSerializer.serialize(kotlinExtraParams.getAnnotationProcessingClassPath()));

    kotlinExtraParams.getExtraClassPaths().stream()
        .map(AbsPathSerializer::serialize)
        .forEach(builder::addExtraClassPaths);

    builder.setAnnotationProcessingTool(
        AnnotationProcessingToolSerializer.serialize(
            kotlinExtraParams.getAnnotationProcessingTool()));
    builder.addAllExtraKotlincArguments(kotlinExtraParams.getExtraKotlincArguments());

    kotlinExtraParams.getKotlinCompilerPlugins().entrySet().stream()
        .forEach(
            entry ->
                builder.putKotlinCompilerPlugins(
                    entry.getKey().toString(),
                    PluginParams.newBuilder().putAllParams(entry.getValue()).build()));

    kotlinExtraParams.getKosabiPluginOptions().entrySet().stream()
        .forEach(
            entry ->
                builder.putKosabiPluginOptions(
                    entry.getKey(), AbsPathSerializer.serialize(entry.getValue())));
    kotlinExtraParams
        .getKosabiJvmAbiGenEarlyTerminationMessagePrefix()
        .ifPresent(builder::setKosabiJvmAbiGenEarlyTerminationMessagePrefix);

    kotlinExtraParams.getFriendPaths().stream()
        .map(AbsPathSerializer::serialize)
        .forEach(builder::addFriendPaths);

    kotlinExtraParams.getKotlinHomeLibraries().stream()
        .map(AbsPathSerializer::serialize)
        .forEach(builder::addKotlinHomeLibraries);

    kotlinExtraParams.getJvmTarget().ifPresent(builder::setJvmTarget);

    builder.setShouldVerifySourceOnlyAbiConstraints(
        kotlinExtraParams.getShouldVerifySourceOnlyAbiConstraints());
    builder.setShouldUseJvmAbiGen(kotlinExtraParams.getShouldUseJvmAbiGen());
    builder.setShouldUseStandaloneKosabi(kotlinExtraParams.getShouldUseStandaloneKosabi());
    kotlinExtraParams
        .getJvmAbiGenPlugin()
        .map(AbsPathSerializer::serialize)
        .ifPresent(builder::setJvmAbiGenPlugin);
    builder.setShouldKotlincRunIncrementally(kotlinExtraParams.getShouldKotlincRunIncrementally());
    builder.setShouldIncrementalKotlicRunQe(kotlinExtraParams.getShouldIncrementalKotlicRunQe());
    kotlinExtraParams
        .getIncrementalStateDir()
        .map(AbsPathSerializer::serialize)
        .ifPresent(builder::setIncrementalStateDir);
    kotlinExtraParams
        .getDepTrackerPlugin()
        .map(AbsPathSerializer::serialize)
        .ifPresent(builder::setDepTrackerPlugin);
    builder.setLanguageVersion(kotlinExtraParams.getLanguageVersion().getValue());
    return builder.build();
  }

  /** Protocol buffer model to internal buck representation. */
  public static KotlinExtraParams deserialize(
      com.facebook.buck.cd.model.java.ResolvedJavacOptions resolvedJavacOptions,
      com.facebook.buck.cd.model.kotlin.KotlinExtraParams kotlinExtraParams) {
    return new KotlinExtraParams(
        kotlinExtraParams.getExtraClassPathsList().stream()
            .map(AbsPathSerializer::deserialize)
            .collect(ImmutableList.toImmutableList()),
        AbsPathSerializer.deserialize(kotlinExtraParams.getStandardLibraryClassPath()),
        AbsPathSerializer.deserialize(kotlinExtraParams.getAnnotationProcessingClassPath()),
        AnnotationProcessingToolSerializer.deserialize(
            kotlinExtraParams.getAnnotationProcessingTool()),
        kotlinExtraParams.getExtraKotlincArgumentsList().stream()
            .collect(ImmutableList.toImmutableList()),
        kotlinExtraParams.getKotlinCompilerPluginsMap().entrySet().stream()
            .collect(
                ImmutableMap.toImmutableMap(
                    e -> AbsPathSerializer.deserialize(e.getKey()),
                    e -> ImmutableMap.copyOf(e.getValue().getParamsMap()))),
        kotlinExtraParams.getKosabiPluginOptionsMap().entrySet().stream()
            .collect(
                ImmutableMap.toImmutableMap(
                    Map.Entry::getKey, e -> AbsPathSerializer.deserialize(e.getValue()))),
        Optional.ofNullable(kotlinExtraParams.getKosabiJvmAbiGenEarlyTerminationMessagePrefix()),
        kotlinExtraParams.getFriendPathsList().stream()
            .map(AbsPathSerializer::deserialize)
            .collect(ImmutableSortedSet.toImmutableSortedSet(AbsPath.comparator())),
        kotlinExtraParams.getKotlinHomeLibrariesList().stream()
            .map(AbsPathSerializer::deserialize)
            .collect(ImmutableSortedSet.toImmutableSortedSet(AbsPath.comparator()))
            .stream()
            .collect(ImmutableList.toImmutableList()),
        ResolvedJavacOptionsSerializer.deserialize(resolvedJavacOptions),
        Optional.ofNullable(kotlinExtraParams.getJvmTarget()),
        kotlinExtraParams.getShouldUseJvmAbiGen(),
        Optional.of(kotlinExtraParams.getJvmAbiGenPlugin())
            .filter(s -> !s.isEmpty())
            .map(AbsPathSerializer::deserialize),
        kotlinExtraParams.getShouldVerifySourceOnlyAbiConstraints(),
        Optional.of(kotlinExtraParams.getDepTrackerPlugin())
            .filter(s -> !s.isEmpty())
            .map(AbsPathSerializer::deserialize),
        kotlinExtraParams.getShouldKotlincRunIncrementally(),
        kotlinExtraParams.getShouldIncrementalKotlicRunQe(),
        kotlinExtraParams.getShouldUseStandaloneKosabi(),
        Optional.of(kotlinExtraParams.getIncrementalStateDir())
            .filter(s -> !s.isEmpty())
            .map(AbsPathSerializer::deserialize),
        kotlinExtraParams.getLanguageVersion());
  }
}
