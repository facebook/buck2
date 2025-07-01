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
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlinSourceChanges;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import org.junit.Before;
import org.junit.Test;

public class KotlinSourceChangesFactoryTest {

  private ActionMetadata mockActionMetadata;
  private AbsPath rootProjectDir;

  @Before
  public void setUp() {
    mockActionMetadata = mock(ActionMetadata.class);
    rootProjectDir = AbsPath.get("/");
  }

  @Test
  public void when_file_addition_then_added_to_modified_files() {
    when(mockActionMetadata.getCurrentSourceFilesDigest())
        .thenReturn(
            new HashMap<Path, String>() {
              {
                put(Paths.get("A.jar"), "1");
                put(Paths.get("a.kt"), "1");
                put(Paths.get("B.jar"), "2");
                put(Paths.get("b.kt"), "2");
                put(Paths.get("c.sh"), "3");
                put(Paths.get("d.java"), "3");
                put(Paths.get("e.java"), "2");
              }
            });
    when(mockActionMetadata.getPreviousSourceFilesDigest())
        .thenReturn(
            new HashMap<Path, String>() {
              {
                put(Paths.get("A.jar"), "1");
                put(Paths.get("a.kt"), "1");
                put(Paths.get("c.sh"), "3");
                put(Paths.get("e.java"), "3");
              }
            });

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
    when(mockActionMetadata.getCurrentSourceFilesDigest())
        .thenReturn(
            new HashMap<Path, String>() {
              {
                put(Paths.get("A.jar"), "1");
                put(Paths.get("a.kt"), "1");
                put(Paths.get("B.jar"), "2");
                put(Paths.get("b.kt"), "2");
                put(Paths.get("c.sh"), "3");
                put(Paths.get("d.java"), "4");
              }
            });
    when(mockActionMetadata.getPreviousSourceFilesDigest())
        .thenReturn(
            new HashMap<Path, String>() {
              {
                put(Paths.get("A.jar"), "1");
                put(Paths.get("a.kt"), "1");
                put(Paths.get("B.jar"), "2");
                put(Paths.get("b.kt"), "3");
                put(Paths.get("c.sh"), "3");
                put(Paths.get("d.java"), "3");
              }
            });

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
    when(mockActionMetadata.getCurrentSourceFilesDigest())
        .thenReturn(
            new HashMap<Path, String>() {
              {
                put(Paths.get("A.jar"), "1");
                put(Paths.get("a.kt"), "1");
                put(Paths.get("c.sh"), "3");
              }
            });
    when(mockActionMetadata.getPreviousSourceFilesDigest())
        .thenReturn(
            new HashMap<Path, String>() {
              {
                put(Paths.get("A.jar"), "1");
                put(Paths.get("a.kt"), "1");
                put(Paths.get("B.jar"), "2");
                put(Paths.get("b.kt"), "3");
                put(Paths.get("c.sh"), "3");
                put(Paths.get("d.java"), "3");
              }
            });

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
