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
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlinSourceChanges;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import org.junit.Before;
import org.junit.Test;

public class KotlinSourceChangesFactoryTest {

  private SourceFilesActionMetadata mockActionMetadata;
  private AbsPath rootProjectDir;

  @Before
  public void setUp() {
    mockActionMetadata = mock(SourceFilesActionMetadata.class);
    rootProjectDir = AbsPath.get("/");
  }

  @Test
  public void when_file_addition_then_added_to_modified_files() {
    when(mockActionMetadata.calculateAddedAndModifiedSourceFiles())
        .thenReturn(Arrays.asList(Paths.get("b.kt"), Paths.get("d.java")));
    when(mockActionMetadata.calculateRemovedFiles()).thenReturn(Collections.emptyList());

    KotlinSourceChanges sourceChanges =
        KotlinSourceChangesFactory.create(rootProjectDir, mockActionMetadata);

    assertTrue(sourceChanges instanceof KotlinSourceChanges.Known);
    assertTrue(
        ((KotlinSourceChanges.Known) sourceChanges)
            .getAddedAndModifiedFiles()
            .contains(rootProjectDir.resolve("b.kt")));
    assertTrue(
        ((KotlinSourceChanges.Known) sourceChanges)
            .getAddedAndModifiedFiles()
            .contains(rootProjectDir.resolve("d.java")));
    assertTrue(((KotlinSourceChanges.Known) sourceChanges).getRemovedFiles().isEmpty());
  }

  @Test
  public void when_file_change_then_added_to_modified_files() {
    when(mockActionMetadata.calculateAddedAndModifiedSourceFiles())
        .thenReturn(Arrays.asList(Paths.get("b.kt"), Paths.get("d.java")));
    when(mockActionMetadata.calculateRemovedFiles()).thenReturn(Collections.emptyList());

    KotlinSourceChanges sourceChanges =
        KotlinSourceChangesFactory.create(rootProjectDir, mockActionMetadata);

    assertTrue(sourceChanges instanceof KotlinSourceChanges.Known);
    assertTrue(
        ((KotlinSourceChanges.Known) sourceChanges)
            .getAddedAndModifiedFiles()
            .contains(rootProjectDir.resolve("b.kt")));
    assertTrue(
        ((KotlinSourceChanges.Known) sourceChanges)
            .getAddedAndModifiedFiles()
            .contains(rootProjectDir.resolve("d.java")));
    assertTrue(((KotlinSourceChanges.Known) sourceChanges).getRemovedFiles().isEmpty());
  }

  @Test
  public void when_file_removal_then_added_to_removed_files() {
    when(mockActionMetadata.calculateAddedAndModifiedSourceFiles())
        .thenReturn(Collections.emptyList());
    when(mockActionMetadata.calculateRemovedFiles())
        .thenReturn(Arrays.asList(Paths.get("b.kt"), Paths.get("d.java")));

    KotlinSourceChanges sourceChanges =
        KotlinSourceChangesFactory.create(rootProjectDir, mockActionMetadata);

    assertTrue(sourceChanges instanceof KotlinSourceChanges.Known);
    assertTrue(
        ((KotlinSourceChanges.Known) sourceChanges)
            .getRemovedFiles()
            .contains(rootProjectDir.resolve("b.kt")));
    assertTrue(
        ((KotlinSourceChanges.Known) sourceChanges)
            .getRemovedFiles()
            .contains(rootProjectDir.resolve("d.java")));
    assertTrue(((KotlinSourceChanges.Known) sourceChanges).getAddedAndModifiedFiles().isEmpty());
  }
}
