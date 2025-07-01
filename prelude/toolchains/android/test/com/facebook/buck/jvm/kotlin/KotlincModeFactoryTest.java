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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.abtesting.ExperimentConfig;
import com.facebook.buck.jvm.kotlin.abtesting.ExperimentConfigService;
import com.facebook.buck.jvm.kotlin.abtesting.ksic.KsicExperimentConstantsKt;
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
  private ExperimentConfigService experimentConfigService;
  private ExperimentConfig experimentConfig;
  private IncrementalCompilationValidator incrementalCompilationValidator;
  private AbsPath absPath;
  private RelPath relPath;

  @Rule public TemporaryPaths temporaryPaths = new TemporaryPaths();

  @Before
  public void setUp() {
    mockKotlinExtraParams = mock(KotlinExtraParams.class);
    mockActionMetadata = mock(ActionMetadata.class);
    experimentConfigService = mock(ExperimentConfigService.class);
    experimentConfig = mock(ExperimentConfig.class);
    incrementalCompilationValidator = mock(IncrementalCompilationValidator.class);
    absPath = AbsPath.get("/");
    relPath = RelPath.get("");

    when(mockActionMetadata.getPreviousDigest()).thenReturn(new HashMap<>());
    when(mockActionMetadata.getCurrentDigest()).thenReturn(new HashMap<>());
    when(experimentConfigService.loadConfig(KsicExperimentConstantsKt.UNIVERSE_NAME))
        .thenReturn(experimentConfig);
    when(incrementalCompilationValidator.validate(mockActionMetadata, absPath, absPath))
        .thenReturn(null);
  }

  @Test
  public void when_sourceOnly_then_nonIncremental() {
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getShouldIncrementalKotlicRunQe()).thenReturn(false);

    KotlincMode kotlincMode =
        new KotlincModeFactory(incrementalCompilationValidator, experimentConfigService)
            .create(
                true,
                absPath,
                absPath,
                true,
                relPath,
                mockKotlinExtraParams,
                Optional.empty(),
                ImmutableList.of());

    assertTrue(kotlincMode instanceof KotlincMode.NonIncremental);
  }

  @Test
  public void when_not_shouldKotlincRunIncrementally_then_nonIncremental() {
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(false);
    when(mockKotlinExtraParams.getShouldIncrementalKotlicRunQe()).thenReturn(false);

    KotlincMode kotlincMode =
        new KotlincModeFactory(incrementalCompilationValidator, experimentConfigService)
            .create(
                false,
                absPath,
                absPath,
                true,
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
    when(mockKotlinExtraParams.getShouldIncrementalKotlicRunQe()).thenReturn(false);
    when(experimentConfig.getBoolParam(KsicExperimentConstantsKt.PARAM_KSIC_ENABLED, true))
        .thenReturn(true);

    new KotlincModeFactory(incrementalCompilationValidator, experimentConfigService)
        .create(
            false,
            absPath,
            absPath,
            true,
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
    when(mockKotlinExtraParams.getShouldIncrementalKotlicRunQe()).thenReturn(false);
    when(experimentConfig.getBoolParam(KsicExperimentConstantsKt.PARAM_KSIC_ENABLED, true))
        .thenReturn(true);

    new KotlincModeFactory(incrementalCompilationValidator, experimentConfigService)
        .create(
            false,
            absPath,
            absPath,
            true,
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
    when(mockKotlinExtraParams.getJvmAbiGenWorkingDir()).thenReturn(Optional.of(AbsPath.get("/")));
    when(mockKotlinExtraParams.getShouldIncrementalKotlicRunQe()).thenReturn(false);
    when(experimentConfig.getBoolParam(KsicExperimentConstantsKt.PARAM_KSIC_ENABLED, true))
        .thenReturn(true);

    KotlincMode kotlincMode =
        new KotlincModeFactory(incrementalCompilationValidator, experimentConfigService)
            .create(
                false,
                absPath,
                absPath,
                true,
                relPath,
                mockKotlinExtraParams,
                Optional.of(mockActionMetadata),
                ImmutableList.of());

    assertTrue(kotlincMode instanceof KotlincMode.Incremental);
  }

  @Test
  public void when_qe2_enabled_and_control_group_then_nonIncremental() throws IOException {
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir())
        .thenReturn(Optional.of(temporaryPaths.newFolder("incrementalStateDir")));
    when(mockKotlinExtraParams.getKotlincWorkingDir()).thenReturn(Optional.empty());
    when(mockKotlinExtraParams.getShouldIncrementalKotlicRunQe()).thenReturn(true);
    when(experimentConfig.getBoolParam(KsicExperimentConstantsKt.PARAM_KSIC_ENABLED, true))
        .thenReturn(false);

    KotlincMode kotlincMode =
        new KotlincModeFactory(incrementalCompilationValidator, experimentConfigService)
            .create(
                false,
                absPath,
                absPath,
                true,
                relPath,
                mockKotlinExtraParams,
                Optional.of(mockActionMetadata),
                ImmutableList.of());

    assertTrue(kotlincMode instanceof KotlincMode.NonIncremental);
  }

  @Test
  public void when_qe2_disabled_and_control_group_then_incremental() throws IOException {
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir())
        .thenReturn(Optional.of(temporaryPaths.newFolder("incrementalStateDir")));
    when(mockKotlinExtraParams.getKotlincWorkingDir()).thenReturn(Optional.of(AbsPath.get("/")));
    when(mockKotlinExtraParams.getShouldIncrementalKotlicRunQe()).thenReturn(false);
    when(experimentConfig.getBoolParam(KsicExperimentConstantsKt.PARAM_KSIC_ENABLED, true))
        .thenReturn(false);

    KotlincMode kotlincMode =
        new KotlincModeFactory(incrementalCompilationValidator, experimentConfigService)
            .create(
                false,
                absPath,
                absPath,
                true,
                relPath,
                mockKotlinExtraParams,
                Optional.of(mockActionMetadata),
                ImmutableList.of());

    assertTrue(kotlincMode instanceof KotlincMode.Incremental);
  }

  @Test
  public void when_qe2_enabled_and_test_group_then_incremental() throws IOException {
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir())
        .thenReturn(Optional.of(temporaryPaths.newFolder("incrementalStateDir")));
    when(mockKotlinExtraParams.getKotlincWorkingDir()).thenReturn(Optional.of(AbsPath.get("/")));
    when(mockKotlinExtraParams.getShouldIncrementalKotlicRunQe()).thenReturn(true);
    when(experimentConfig.getBoolParam(KsicExperimentConstantsKt.PARAM_KSIC_ENABLED, true))
        .thenReturn(true);

    KotlincMode kotlincMode =
        new KotlincModeFactory(incrementalCompilationValidator, experimentConfigService)
            .create(
                false,
                absPath,
                absPath,
                true,
                relPath,
                mockKotlinExtraParams,
                Optional.of(mockActionMetadata),
                ImmutableList.of());

    assertTrue(kotlincMode instanceof KotlincMode.Incremental);
  }

  @Test
  public void when_qe2_disabled_and_test_group_then_incremental() throws IOException {
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir())
        .thenReturn(Optional.of(temporaryPaths.newFolder("incrementalStateDir")));
    when(mockKotlinExtraParams.getKotlincWorkingDir()).thenReturn(Optional.of(AbsPath.get("/")));
    when(mockKotlinExtraParams.getShouldIncrementalKotlicRunQe()).thenReturn(false);
    when(experimentConfig.getBoolParam(KsicExperimentConstantsKt.PARAM_KSIC_ENABLED, true))
        .thenReturn(true);

    KotlincMode kotlincMode =
        new KotlincModeFactory(incrementalCompilationValidator, experimentConfigService)
            .create(
                false,
                absPath,
                absPath,
                true,
                relPath,
                mockKotlinExtraParams,
                Optional.of(mockActionMetadata),
                ImmutableList.of());

    assertTrue(kotlincMode instanceof KotlincMode.Incremental);
  }

  @Test
  public void when_control_group_then_incrementalStateDir_is_cleared() throws IOException {
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getShouldIncrementalKotlicRunQe()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir())
        .thenReturn(Optional.of(temporaryPaths.newFolder("incrementalStateDir")));
    boolean fileCreated =
        temporaryPaths
            .getRoot()
            .resolve("incrementalStateDir")
            .resolve("kotlinWorkingDir")
            .toFile()
            .createNewFile();
    assertTrue(fileCreated);
    when(mockKotlinExtraParams.getKotlincWorkingDir()).thenReturn(Optional.empty());
    when(experimentConfig.getBoolParam(KsicExperimentConstantsKt.PARAM_KSIC_ENABLED, true))
        .thenReturn(false);
    String[] filesBefore = temporaryPaths.getRoot().resolve("incrementalStateDir").toFile().list();
    assertNotNull(filesBefore);
    assertEquals(1, filesBefore.length);

    new KotlincModeFactory(incrementalCompilationValidator, experimentConfigService)
        .create(
            false,
            absPath,
            absPath,
            true,
            relPath,
            mockKotlinExtraParams,
            Optional.of(mockActionMetadata),
            ImmutableList.of());

    String[] filesAfter = temporaryPaths.getRoot().resolve("incrementalStateDir").toFile().list();
    assertNotNull(filesAfter);
    assertEquals(0, filesAfter.length);
  }

  @Test
  public void when_is_track_class_usage_disabled_then_does_not_require_rebuild()
      throws IOException {
    when(mockKotlinExtraParams.getShouldKotlincRunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir())
        .thenReturn(Optional.of(temporaryPaths.newFolder("incrementalStateDir")));
    when(mockKotlinExtraParams.getKotlincWorkingDir()).thenReturn(Optional.of(AbsPath.get("/")));
    when(mockKotlinExtraParams.getShouldIncrementalKotlicRunQe()).thenReturn(false);

    KotlincMode kotlincMode =
        new KotlincModeFactory(incrementalCompilationValidator, experimentConfigService)
            .create(
                false,
                absPath,
                absPath,
                false,
                relPath,
                mockKotlinExtraParams,
                Optional.of(mockActionMetadata),
                ImmutableList.of());

    assertTrue(kotlincMode instanceof KotlincMode.Incremental);
    assertNull(((KotlincMode.Incremental) kotlincMode).getRebuildReason());
  }
}
