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

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode;
import com.facebook.buck.testutil.TemporaryPaths;
import com.google.common.collect.ImmutableList;
import java.io.IOException;
import java.util.HashMap;
import java.util.Optional;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

public class KotlincModeFactoryTest {

  private KotlinExtraParams mockKotlinExtraParams;
  private ActionMetadata mockActionMetadata;
  private IncrementalCompilationValidator incrementalCompilationValidator;
  private AbsPath absPath;
  private RelPath relPath;

  @Rule public TemporaryPaths temporaryPaths = new TemporaryPaths();

  @Before
  public void setUp() {
    mockKotlinExtraParams = mock(KotlinExtraParams.class);
    mockActionMetadata = mock(ActionMetadata.class);
    incrementalCompilationValidator = mock(IncrementalCompilationValidator.class);
    absPath = AbsPath.get("/");
    relPath = RelPath.get("");

    when(mockActionMetadata.getPreviousDigest()).thenReturn(new HashMap<>());
    when(mockActionMetadata.getCurrentDigest()).thenReturn(new HashMap<>());
    when(incrementalCompilationValidator.validate(mockActionMetadata, absPath, absPath, absPath))
        .thenReturn(null);
  }

  @Test
  public void when_sourceOnly_then_nonIncremental() {
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);

    KotlincMode kotlincMode =
        new KotlincModeFactory(incrementalCompilationValidator)
            .create(
                true,
                absPath,
                absPath,
                true,
                relPath,
                relPath,
                relPath,
                mockKotlinExtraParams,
                Optional.empty(),
                ImmutableList.of());

    assertTrue(kotlincMode instanceof KotlincMode.NonIncremental);
  }

  @Test
  public void when_not_shouldKotlincRunIncrementally_then_nonIncremental() {
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(false);

    KotlincMode kotlincMode =
        new KotlincModeFactory(incrementalCompilationValidator)
            .create(
                false,
                absPath,
                absPath,
                true,
                relPath,
                relPath,
                relPath,
                mockKotlinExtraParams,
                Optional.empty(),
                ImmutableList.of());

    assertTrue(kotlincMode instanceof KotlincMode.NonIncremental);
  }

  @Test(expected = IllegalStateException.class)
  public void when_incrementalStateDir_empty_then_error() {
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir()).thenReturn(Optional.of(AbsPath.get("/")));
    when(mockKotlinExtraParams.getKotlincWorkingDir()).thenReturn(Optional.empty());

    new KotlincModeFactory(incrementalCompilationValidator)
        .create(
            false,
            absPath,
            absPath,
            true,
            relPath,
            relPath,
            relPath,
            mockKotlinExtraParams,
            Optional.of(mockActionMetadata),
            ImmutableList.of());
  }

  @Test(expected = IllegalStateException.class)
  public void when_metadata_null_then_error() {
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir()).thenReturn(Optional.of(AbsPath.get("/")));
    when(mockKotlinExtraParams.getKotlincWorkingDir()).thenReturn(Optional.empty());

    new KotlincModeFactory(incrementalCompilationValidator)
        .create(
            false,
            absPath,
            absPath,
            true,
            relPath,
            relPath,
            relPath,
            mockKotlinExtraParams,
            Optional.empty(),
            ImmutableList.of());
  }

  @Test
  public void
      when_shouldKotlincRunIncrementally_and_incrementalStateDir_isPresent_then_incremental() {
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir()).thenReturn(Optional.of(AbsPath.get("/")));
    when(mockKotlinExtraParams.getKotlincWorkingDir()).thenReturn(Optional.of(AbsPath.get("/")));

    KotlincMode kotlincMode =
        new KotlincModeFactory(incrementalCompilationValidator)
            .create(
                false,
                absPath,
                absPath,
                true,
                relPath,
                relPath,
                relPath,
                mockKotlinExtraParams,
                Optional.of(mockActionMetadata),
                ImmutableList.of());

    assertTrue(kotlincMode instanceof KotlincMode.Incremental);
  }

  @Test
  public void when_is_track_class_usage_disabled_then_does_not_require_rebuild()
      throws IOException {
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir())
        .thenReturn(Optional.of(temporaryPaths.newFolder("incrementalStateDir")));
    when(mockKotlinExtraParams.getKotlincWorkingDir()).thenReturn(Optional.of(AbsPath.get("/")));

    KotlincMode kotlincMode =
        new KotlincModeFactory(incrementalCompilationValidator)
            .create(
                false,
                absPath,
                absPath,
                false,
                relPath,
                relPath,
                relPath,
                mockKotlinExtraParams,
                Optional.of(mockActionMetadata),
                ImmutableList.of());

    assertTrue(kotlincMode instanceof KotlincMode.Incremental);
    assertNull(((KotlincMode.Incremental) kotlincMode).getRebuildReason());
  }
}
