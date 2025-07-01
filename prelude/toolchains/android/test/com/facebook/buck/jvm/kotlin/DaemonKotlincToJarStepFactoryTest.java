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

import static com.facebook.buck.jvm.kotlin.CompilerPluginUtils.KOTLIN_PLUGIN_OUT_PLACEHOLDER;
import static com.facebook.buck.jvm.kotlin.CompilerPluginUtils.getKotlinCompilerPluginsArgs;
import static com.facebook.buck.jvm.kotlin.DaemonKotlincToJarStepFactory.getRunsOnJavaOnlyProcessors;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.java.JavacLanguageLevelOptions;
import com.facebook.buck.jvm.java.JavacPluginParams;
import com.facebook.buck.jvm.java.ResolvedJavacOptions;
import com.facebook.buck.jvm.java.ResolvedJavacPluginProperties;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;
import org.junit.Test;

public class DaemonKotlincToJarStepFactoryTest {
  @Test
  public void test_getKotlinCompilerPluginsArgs_noParams() {
    AbsPath pluginPath = new FakePath("/plugin.jar");
    ImmutableMap<String, String> pluginParams = ImmutableMap.of();
    String outputDir = "default/codegen/output/dir";

    ImmutableList<String> compilerPluginsArgs =
        getKotlinCompilerPluginsArgs(pluginPath, pluginParams, outputDir);

    assertEquals(compilerPluginsArgs, ImmutableList.of("-Xplugin=" + pluginPath));
  }

  @Test
  public void test_getKotlinCompilerPluginsArgs_withParams() {
    AbsPath pluginPath = new FakePath("/plugin.jar");
    ImmutableMap<String, String> pluginParams =
        ImmutableMap.of("param1", "value1", "param2", "value2");
    String outputDir = "default/codegen/output/dir";

    ImmutableList<String> compilerPluginsArgs =
        getKotlinCompilerPluginsArgs(pluginPath, pluginParams, outputDir);

    assertEquals(
        compilerPluginsArgs,
        ImmutableList.of("-Xplugin=" + pluginPath, "-P", "param1=value1,param2=value2"));
  }

  @Test
  public void test_getKotlinCompilerPluginsArgs_codegenDirPlaceholderGetsReplaced() {
    AbsPath pluginPath = new FakePath("/plugin.jar");
    ImmutableMap<String, String> pluginParams =
        ImmutableMap.of("param1", KOTLIN_PLUGIN_OUT_PLACEHOLDER);
    String outputDir = "default/codegen/output/dir";

    ImmutableList<String> compilerPluginsArgs =
        getKotlinCompilerPluginsArgs(pluginPath, pluginParams, outputDir);

    assertEquals(
        compilerPluginsArgs,
        ImmutableList.of("-Xplugin=" + pluginPath, "-P", "param1=" + outputDir));
  }

  @Test
  public void test_getRunsOnJavaOnlyProcessors_includeJavaOnlyProcessors() {
    ResolvedJavacPluginProperties javaOnlyPlugin = getJavaOnlyPlugin();
    ResolvedJavacPluginProperties usualPlugin = getUsualPlugin();
    JavacPluginParams javacPluginParams =
        new JavacPluginParams(
            ImmutableList.of(javaOnlyPlugin, usualPlugin), ImmutableSortedSet.of());
    ResolvedJavacOptions resolvedJavacOptions = getResolvedJavacOptions(javacPluginParams);

    JavacPluginParams filteredJavacPluginParams = getRunsOnJavaOnlyProcessors(resolvedJavacOptions);

    assertTrue(filteredJavacPluginParams.getPluginProperties().contains(javaOnlyPlugin));
    assertFalse(filteredJavacPluginParams.getPluginProperties().contains(usualPlugin));
  }

  @Test
  public void test_getKaptProcessors_excludeJavaOnlyProcessors() {
    ResolvedJavacPluginProperties javaOnlyPlugin = getJavaOnlyPlugin();
    ResolvedJavacPluginProperties usualPlugin = getUsualPlugin();

    ImmutableList<ResolvedJavacPluginProperties> filteredPluginProperties =
        KaptStepsBuilder.getKaptAnnotationProcessors(ImmutableList.of(javaOnlyPlugin, usualPlugin));

    assertFalse(filteredPluginProperties.contains(javaOnlyPlugin));
    assertTrue(filteredPluginProperties.contains(usualPlugin));
  }

  private static final class FakePath implements AbsPath {
    private final Path path;

    public FakePath(String path) {
      this.path = Paths.get(path);
    }

    @Override
    public Path getPath() {
      return path;
    }
  }

  private static ResolvedJavacPluginProperties getUsualPlugin() {
    return getPluginProperties(
        false /* runsOnJavaOnly */, "com.example.UsualPlugin", "some/path/to_other.jar");
  }

  private static ResolvedJavacPluginProperties getJavaOnlyPlugin() {
    return getPluginProperties(
        true /* runsOnJavaOnly */, "com.example.JavaOnlyPlugin", "some/path/to.jar");
  }

  private static ResolvedJavacPluginProperties getPluginProperties(
      boolean runsOnJavaOnly, String processorName, String processorPath) {
    return new ResolvedJavacPluginProperties(
        true /* canReuseClassLoader */,
        false /* doesNotAffectAbi */,
        false /* supportsAbiGenerationFromSource */,
        runsOnJavaOnly /* runsOnJavaOnly */,
        ImmutableSortedSet.of(processorName),
        ImmutableList.of(RelPath.get(processorPath)),
        ImmutableMap.of() /* pathParams */,
        ImmutableList.of() /* arguments */);
  }

  private static ResolvedJavacOptions getResolvedJavacOptions(
      JavacPluginParams javaAnnotationProcessorParams) {
    return new ResolvedJavacOptions(
        Optional.ofNullable(null),
        ImmutableList.of() /* bootclasspathList */,
        JavacLanguageLevelOptions.DEFAULT,
        false /* debug */,
        false /* verbose */,
        javaAnnotationProcessorParams,
        JavacPluginParams.EMPTY /* standardJavacPluginParams */,
        ImmutableList.of() /* extraArguments */,
        null /* systemImage */);
  }
}
