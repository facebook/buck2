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

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.ksp.incremental.Ksp2Mode;
import com.facebook.buck.testutil.TemporaryPaths;
import java.util.Optional;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

public class Ksp2ModeFactoryTest {

  private KotlinExtraParams mockKotlinExtraParams;
  private ActionMetadata mockActionMetadata;
  private RelPath relPath;

  @Rule public TemporaryPaths temporaryPaths = new TemporaryPaths();

  @Before
  public void setUp() {
    mockKotlinExtraParams = mock(KotlinExtraParams.class);
    mockActionMetadata = mock(ActionMetadata.class);
    relPath = RelPath.get("");
  }

  @Test
  public void when_sourceOnly_then_nonIncremental() {
    when(mockKotlinExtraParams.getShouldKsp2RunIncrementally()).thenReturn(true);

    Ksp2Mode ksp2Mode =
        Ksp2ModeFactory.create(true, relPath, mockKotlinExtraParams, Optional.empty());

    assertTrue(ksp2Mode instanceof Ksp2Mode.NonIncremental);
  }

  @Test
  public void when_not_shouldKsp2RunIncrementally_then_nonIncremental() {
    when(mockKotlinExtraParams.getShouldKsp2RunIncrementally()).thenReturn(false);

    Ksp2Mode ksp2Mode =
        Ksp2ModeFactory.create(false, relPath, mockKotlinExtraParams, Optional.empty());

    assertTrue(ksp2Mode instanceof Ksp2Mode.NonIncremental);
  }

  @Test(expected = IllegalStateException.class)
  public void when_incrementalStateDir_empty_then_error() {
    when(mockKotlinExtraParams.getShouldKsp2RunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir()).thenReturn(Optional.of(AbsPath.get("/")));
    when(mockKotlinExtraParams.getKsp2CachesDir()).thenReturn(Optional.empty());

    Ksp2ModeFactory.create(false, relPath, mockKotlinExtraParams, Optional.of(mockActionMetadata));
  }

  @Test(expected = IllegalStateException.class)
  public void when_metadata_null_then_error() {
    when(mockKotlinExtraParams.getShouldKsp2RunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir()).thenReturn(Optional.of(AbsPath.get("/")));
    when(mockKotlinExtraParams.getKsp2CachesDir()).thenReturn(Optional.empty());

    Ksp2ModeFactory.create(false, relPath, mockKotlinExtraParams, Optional.empty());
  }

  @Test
  public void
      when_shouldKotlincRunIncrementally_and_incrementalStateDir_isPresent_then_incremental() {
    when(mockKotlinExtraParams.getShouldKsp2RunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getIncrementalStateDir()).thenReturn(Optional.of(AbsPath.get("/")));
    when(mockKotlinExtraParams.getKsp2CachesDir()).thenReturn(Optional.of(AbsPath.get("/")));

    Ksp2Mode ksp2Mode =
        Ksp2ModeFactory.create(
            false, relPath, mockKotlinExtraParams, Optional.of(mockActionMetadata));

    assertTrue(ksp2Mode instanceof Ksp2Mode.Incremental);
  }
}
