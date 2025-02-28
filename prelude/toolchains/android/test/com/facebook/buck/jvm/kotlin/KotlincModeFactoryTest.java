/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin;

import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlincMode;
import com.google.common.collect.ImmutableList;
import java.util.HashMap;
import java.util.Optional;
import org.easymock.EasyMock;
import org.junit.Before;
import org.junit.Test;

public class KotlincModeFactoryTest {
  private KotlinExtraParams mockKotlinExtraParams;
  private ActionMetadata mockActionMetadata;
  private AbsPath absPath;
  private RelPath relPath;

  @Before
  public void setUp() {
    mockKotlinExtraParams = EasyMock.createMock(KotlinExtraParams.class);
    mockActionMetadata = EasyMock.createMock(ActionMetadata.class);
    absPath = AbsPath.get("/");
    relPath = RelPath.get("");

    expect(mockActionMetadata.getPreviousDigest()).andReturn(new HashMap<>());
    expect(mockActionMetadata.getCurrentDigest()).andReturn(new HashMap<>());
    replay(mockActionMetadata);
  }

  @Test
  public void when_sourceOnly_then_nonIncremental() {
    expect(mockKotlinExtraParams.shouldKotlincRunViaBuildToolsApi()).andReturn(true);
    expect(mockKotlinExtraParams.shouldKotlincRunIncrementally()).andReturn(true);
    replay(mockKotlinExtraParams);

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
    expect(mockKotlinExtraParams.shouldKotlincRunViaBuildToolsApi()).andReturn(false);
    expect(mockKotlinExtraParams.shouldKotlincRunIncrementally()).andReturn(true);
    replay(mockKotlinExtraParams);

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
    expect(mockKotlinExtraParams.shouldKotlincRunViaBuildToolsApi()).andReturn(true);
    expect(mockKotlinExtraParams.shouldKotlincRunIncrementally()).andReturn(false);
    replay(mockKotlinExtraParams);

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
    expect(mockKotlinExtraParams.shouldKotlincRunViaBuildToolsApi()).andReturn(true);
    expect(mockKotlinExtraParams.shouldKotlincRunIncrementally()).andReturn(true);
    expect(mockKotlinExtraParams.getIncrementalStateDir()).andReturn(Optional.of(AbsPath.get("/")));
    expect(mockKotlinExtraParams.getKotlincWorkingDir()).andReturn(Optional.empty());
    replay(mockKotlinExtraParams);

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
    expect(mockKotlinExtraParams.shouldKotlincRunViaBuildToolsApi()).andReturn(true);
    expect(mockKotlinExtraParams.shouldKotlincRunIncrementally()).andReturn(true);
    expect(mockKotlinExtraParams.getIncrementalStateDir()).andReturn(Optional.of(AbsPath.get("/")));
    expect(mockKotlinExtraParams.getKotlincWorkingDir()).andReturn(Optional.empty());
    replay(mockKotlinExtraParams);

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
    expect(mockKotlinExtraParams.shouldKotlincRunViaBuildToolsApi()).andReturn(true);
    expect(mockKotlinExtraParams.shouldKotlincRunIncrementally()).andReturn(true);
    expect(mockKotlinExtraParams.getIncrementalStateDir()).andReturn(Optional.of(AbsPath.get("/")));
    expect(mockKotlinExtraParams.getKotlincWorkingDir()).andReturn(Optional.of(AbsPath.get("/")));
    expect(mockKotlinExtraParams.getJvmAbiGenWorkingDir()).andReturn(Optional.of(AbsPath.get("/")));

    replay(mockKotlinExtraParams);

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
