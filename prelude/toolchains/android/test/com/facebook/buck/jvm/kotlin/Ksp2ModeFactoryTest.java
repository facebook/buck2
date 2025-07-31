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
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.filesystems.RelPath;
import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.ksp.incremental.Ksp2Mode;
import com.facebook.buck.testutil.TemporaryPaths;
import com.google.common.collect.ImmutableMap;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

public class Ksp2ModeFactoryTest {

  private KotlinExtraParams mockKotlinExtraParams;
  private ActionMetadata mockActionMetadata;
  private RelPath relPath;
  private AbsPath cachesDir;
  private AbsPath absPath;

  @Rule public TemporaryPaths temporaryPaths = new TemporaryPaths();

  @Before
  public void setUp() {
    mockKotlinExtraParams = mock(KotlinExtraParams.class);
    mockActionMetadata = mock(ActionMetadata.class);
    relPath = RelPath.get("output");
    cachesDir = temporaryPaths.getRoot().resolve("caches");
    absPath = AbsPath.get("/");
  }

  @Test
  public void when_sourceOnly_then_nonIncremental() {
    when(mockKotlinExtraParams.getShouldKsp2RunIncrementally()).thenReturn(true);

    Ksp2Mode ksp2Mode =
        Ksp2ModeFactory.create(absPath, true, relPath, mockKotlinExtraParams, mockActionMetadata);

    assertTrue(ksp2Mode instanceof Ksp2Mode.NonIncremental);
    assertEquals(relPath, ((Ksp2Mode.NonIncremental) ksp2Mode).getKspCachesOutput());
  }

  @Test
  public void when_not_shouldKsp2RunIncrementally_then_nonIncremental() {
    when(mockKotlinExtraParams.getShouldKsp2RunIncrementally()).thenReturn(false);

    Ksp2Mode ksp2Mode =
        Ksp2ModeFactory.create(absPath, false, relPath, mockKotlinExtraParams, null);

    assertTrue(ksp2Mode instanceof Ksp2Mode.NonIncremental);
    assertEquals(relPath, ((Ksp2Mode.NonIncremental) ksp2Mode).getKspCachesOutput());
  }

  @Test(expected = IllegalStateException.class)
  public void when_ksp2CachesDir_empty_then_error() {
    when(mockKotlinExtraParams.getShouldKsp2RunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getKsp2CachesDir()).thenReturn(Optional.empty());

    Ksp2ModeFactory.create(absPath, false, relPath, mockKotlinExtraParams, mockActionMetadata);
  }

  @Test(expected = IllegalArgumentException.class)
  public void when_actionMetadata_null_in_incremental_mode_then_error() {
    when(mockKotlinExtraParams.getShouldKsp2RunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getKsp2CachesDir()).thenReturn(Optional.of(cachesDir));

    Ksp2ModeFactory.create(absPath, false, relPath, mockKotlinExtraParams, null);
  }

  @Test
  public void when_incremental_conditions_met_then_incremental() {
    when(mockKotlinExtraParams.getShouldKsp2RunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getKsp2CachesDir()).thenReturn(Optional.of(cachesDir));

    Ksp2Mode ksp2Mode =
        Ksp2ModeFactory.create(absPath, false, relPath, mockKotlinExtraParams, mockActionMetadata);

    assertTrue(ksp2Mode instanceof Ksp2Mode.Incremental);
  }

  @Test
  public void when_incremental_mode_then_correct_properties_set() {
    Path addedFile = Paths.get("src/main/kotlin/NewFile.kt");
    Path modifiedFile = Paths.get("src/main/java/ModifiedFile.java");
    Path removedFile = Paths.get("src/main/kotlin/RemovedFile.kt");
    Path unchangedFile = Paths.get("src/main/kotlin/UnchangedFile.kt");
    ImmutableMap<Path, String> previousDigest =
        ImmutableMap.of(
            modifiedFile, "old_digest",
            removedFile, "removed_digest",
            unchangedFile, "unchanged_digest");
    ImmutableMap<Path, String> currentDigest =
        ImmutableMap.of(
            addedFile, "new_digest",
            modifiedFile, "new_digest",
            unchangedFile, "unchanged_digest");
    when(mockActionMetadata.getPreviousDigest()).thenReturn(previousDigest);
    when(mockActionMetadata.getCurrentDigest()).thenReturn(currentDigest);
    when(mockKotlinExtraParams.getShouldKsp2RunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getKsp2CachesDir()).thenReturn(Optional.of(cachesDir));

    Ksp2Mode ksp2Mode =
        Ksp2ModeFactory.create(absPath, false, relPath, mockKotlinExtraParams, mockActionMetadata);

    assertTrue(ksp2Mode instanceof Ksp2Mode.Incremental);
    Ksp2Mode.Incremental incrementalMode = (Ksp2Mode.Incremental) ksp2Mode;
    assertEquals(cachesDir, incrementalMode.getCachesDir());
    assertTrue(incrementalMode.getIncrementalLog());
    assertEquals(2, incrementalMode.getModifiedSources().size());
    assertEquals(1, incrementalMode.getRemovedSources().size());
    assertTrue(incrementalMode.getChangedClasses().isEmpty());
  }

  @Test
  public void when_incremental_mode_with_no_changes_then_empty_lists() {
    Path unchangedFile = Paths.get("src/main/kotlin/UnchangedFile.kt");
    ImmutableMap<Path, String> digest = ImmutableMap.of(unchangedFile, "unchanged_digest");
    when(mockActionMetadata.getPreviousDigest()).thenReturn(digest);
    when(mockActionMetadata.getCurrentDigest()).thenReturn(digest);
    when(mockKotlinExtraParams.getShouldKsp2RunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getKsp2CachesDir()).thenReturn(Optional.of(cachesDir));

    Ksp2Mode ksp2Mode =
        Ksp2ModeFactory.create(absPath, false, relPath, mockKotlinExtraParams, mockActionMetadata);

    assertTrue(ksp2Mode instanceof Ksp2Mode.Incremental);
    Ksp2Mode.Incremental incrementalMode = (Ksp2Mode.Incremental) ksp2Mode;
    assertTrue(incrementalMode.getModifiedSources().isEmpty());
    assertTrue(incrementalMode.getRemovedSources().isEmpty());
    assertTrue(incrementalMode.getChangedClasses().isEmpty());
  }

  @Test
  public void when_incremental_mode_with_non_source_files_then_ignored() {
    Path kotlinFile = Paths.get("src/main/kotlin/File.kt");
    Path javaFile = Paths.get("src/main/java/File.java");
    Path resourceFile = Paths.get("src/main/resources/config.xml");
    Path classFile = Paths.get("build/classes/File.class");
    ImmutableMap<Path, String> previousDigest =
        ImmutableMap.of(
            kotlinFile, "old_digest",
            resourceFile, "resource_digest",
            classFile, "class_digest");
    ImmutableMap<Path, String> currentDigest =
        ImmutableMap.of(
            kotlinFile, "new_digest",
            javaFile, "java_digest",
            resourceFile, "resource_digest",
            classFile, "class_digest");
    when(mockActionMetadata.getPreviousDigest()).thenReturn(previousDigest);
    when(mockActionMetadata.getCurrentDigest()).thenReturn(currentDigest);
    when(mockKotlinExtraParams.getShouldKsp2RunIncrementally()).thenReturn(true);
    when(mockKotlinExtraParams.getKsp2CachesDir()).thenReturn(Optional.of(cachesDir));

    Ksp2Mode ksp2Mode =
        Ksp2ModeFactory.create(absPath, false, relPath, mockKotlinExtraParams, mockActionMetadata);

    assertTrue(ksp2Mode instanceof Ksp2Mode.Incremental);
    Ksp2Mode.Incremental incrementalMode = (Ksp2Mode.Incremental) ksp2Mode;
    // Only kotlin and java files should be considered
    assertEquals(2, incrementalMode.getModifiedSources().size());
    assertEquals(0, incrementalMode.getRemovedSources().size());
  }
}
