/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin;

import static com.facebook.buck.jvm.kotlin.CompilerPluginUtils.KOTLIN_PLUGIN_OUT_PLACEHOLDER;
import static com.facebook.buck.jvm.kotlin.CompilerPluginUtils.getKotlinCompilerPluginsArgs;
import static org.junit.Assert.assertEquals;

import com.facebook.buck.core.filesystems.AbsPath;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import java.nio.file.Path;
import java.nio.file.Paths;
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
}
