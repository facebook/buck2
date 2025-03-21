/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode;
import com.google.common.collect.ImmutableList;
import java.util.HashMap;
import java.util.Optional;
import org.junit.Before;
import org.junit.Test;

public class KotlincModeFactoryTest {
  private KotlinExtraParams mockKotlinExtraParams;
  private ActionMetadata mockActionMetadata;
  private AbsPath absPath;
  private RelPath relPath;

  @Before
  public void setUp() {
    mockKotlinExtraParams = mock(KotlinExtraParams.class);
    mockActionMetadata = mock(ActionMetadata.class);
    absPath = AbsPath.get("/");
    relPath = RelPath.get("");

    when(mockActionMetadata.getPreviousDigest()).thenReturn(new HashMap<>());
    when(mockActionMetadata.getCurrentDigest()).thenReturn(new HashMap<>());
  }

  @Test
  public void when_sourceOnly_then_nonIncremental() {
    when(mockKotlinExtraParams.getShouldKotlincRunViaBuildToolsApi()).thenReturn(true);
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);

    KotlincMode kotlincMode =
        KotlincModeFactory.create(
            true,
            absPath,
            absPath,
            relPath,
            mockKotlinExtraParams,
            Optional.empty(),
            ImmutableList.of());

    assertTrue(kotlincMode instanceof KotlincMode.NonIncremental);
  }

  @Test
  public void when_not_shouldKotlincRunViaBuildToolsApi_then_nonIncremental() {
    when(mockKotlinExtraParams.getShouldKotlincRunViaBuildToolsApi()).thenReturn(false);
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);

    KotlincMode kotlincMode =
        KotlincModeFactory.create(
            false,
            absPath,
            absPath,
            relPath,
            mockKotlinExtraParams,
            Optional.empty(),
            ImmutableList.of());

    assertTrue(kotlincMode instanceof KotlincMode.NonIncremental);
  }

  @Test
  public void when_not_shouldKotlincRunIncrementally_then_nonIncremental() {
    when(mockKotlinExtraParams.getShouldKotlincRunViaBuildToolsApi()).thenReturn(true);
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(false);

    KotlincMode kotlincMode =
        KotlincModeFactory.create(
            false,
            absPath,
            absPath,
            relPath,
            mockKotlinExtraParams,
            Optional.empty(),
            ImmutableList.of());

    assertTrue(kotlincMode instanceof KotlincMode.NonIncremental);
  }

  @Test(expected = IllegalStateException.class)
  public void when_incrementalStateDir_empty_then_error() {
    when(mockKotlinExtraParams.getShouldKotlincRunViaBuildToolsApi()).thenReturn(true);
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir()).thenReturn(Optional.of(AbsPath.get("/")));
    when(mockKotlinExtraParams.getKotlincWorkingDir()).thenReturn(Optional.empty());

    KotlincModeFactory.create(
        false,
        absPath,
        absPath,
        relPath,
        mockKotlinExtraParams,
        Optional.of(mockActionMetadata),
        ImmutableList.of());
  }

  @Test(expected = IllegalStateException.class)
  public void when_metadata_null_then_error() {
    when(mockKotlinExtraParams.getShouldKotlincRunViaBuildToolsApi()).thenReturn(true);
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir()).thenReturn(Optional.of(AbsPath.get("/")));
    when(mockKotlinExtraParams.getKotlincWorkingDir()).thenReturn(Optional.empty());

    KotlincModeFactory.create(
        false,
        absPath,
        absPath,
        relPath,
        mockKotlinExtraParams,
        Optional.empty(),
        ImmutableList.of());
  }

  @Test
  public void
      when_shouldKotlincRunIncrementally_and_incrementalStateDir_isPresent_then_incremental() {
    when(mockKotlinExtraParams.getShouldKotlincRunViaBuildToolsApi()).thenReturn(true);
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir()).thenReturn(Optional.of(AbsPath.get("/")));
    when(mockKotlinExtraParams.getKotlincWorkingDir()).thenReturn(Optional.of(AbsPath.get("/")));
    when(mockKotlinExtraParams.getJvmAbiGenWorkingDir()).thenReturn(Optional.of(AbsPath.get("/")));

    KotlincMode kotlincMode =
        KotlincModeFactory.create(
            false,
            absPath,
            absPath,
            relPath,
            mockKotlinExtraParams,
            Optional.of(mockActionMetadata),
            ImmutableList.of());

    assertTrue(kotlincMode instanceof KotlincMode.Incremental);
  }
}
