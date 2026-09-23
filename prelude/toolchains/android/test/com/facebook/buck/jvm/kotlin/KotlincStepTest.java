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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.core.filesystems.AbsPath;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import java.nio.file.Paths;
import java.util.Optional;
import org.junit.Test;

public class KotlincStepTest {
  private static final AbsPath TEST_ROOT = AbsPath.of(Paths.get(".").toAbsolutePath().normalize());

  @Test
  public void structuredOptionsIncludeContextAndExplicitEmptyClasspath() {
    ImmutableList<String> options =
        KotlincStep.getKosabiApplicabilityPluginOptions(
            "fbcode//example:target", TEST_ROOT, ImmutableList.of(), Optional.of(path("fbcode")));

    assertEquals(
        ImmutableList.of(
            "-P",
            "plugin:com.facebook.kotlin.compilerplugins.kosabiapplicability:target-label=fbcode//example:target",
            "-P",
            "plugin:com.facebook.kotlin.compilerplugins.kosabiapplicability:source-root="
                + TEST_ROOT.getPath(),
            "-P",
            "plugin:com.facebook.kotlin.compilerplugins.kosabiapplicability:source-root-prefix=fbcode",
            "-P",
            "plugin:com.facebook.kotlin.compilerplugins.kosabiapplicability:source-only-abi-classpath="),
        options);
  }

  @Test
  public void structuredOptionsFailClosedWithoutRequiredBuckContext() {
    IllegalStateException missingPlugin =
        assertThrows(
            IllegalStateException.class,
            () -> KotlincStep.getRequiredKosabiApplicabilityPlugin(ImmutableMap.of()));
    assertTrue(missingPlugin.getMessage().contains("plugin path is missing"));

    IllegalStateException missingCellRoot =
        assertThrows(
            IllegalStateException.class,
            () ->
                KotlincStep.getKosabiApplicabilityPluginOptions(
                    "fbcode//example:target", TEST_ROOT, ImmutableList.of(), Optional.empty()));
    assertTrue(missingCellRoot.getMessage().contains("cell root path is missing"));
  }

  private static AbsPath path(String relativePath) {
    return TEST_ROOT.resolve(relativePath);
  }
}
