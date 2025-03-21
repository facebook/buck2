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

import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.KotlinSourceChanges;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import org.junit.Before;
import org.junit.Test;

public class KotlinSourceChangesFactoryTest {

  private ActionMetadata mockActionMetadata;

  @Before
  public void setUp() {
    mockActionMetadata = mock(ActionMetadata.class);
  }

  @Test
  public void when_file_addition_then_added_to_modified_files() {
    when(mockActionMetadata.getCurrentDigest())
        .thenReturn(
            new HashMap<Path, String>() {
              {
                put(Paths.get("A.jar"), "1");
                put(Paths.get("a.kt"), "1");
                put(Paths.get("B.jar"), "2");
                put(Paths.get("b.kt"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    when(mockActionMetadata.getPreviousDigest())
        .thenReturn(
            new HashMap<Path, String>() {
              {
                put(Paths.get("A.jar"), "1");
                put(Paths.get("a.kt"), "1");
                put(Paths.get("c.sh"), "3");
              }
            });

    KotlinSourceChanges sourceChanges = KotlinSourceChangesFactory.create(mockActionMetadata);

    assertTrue(sourceChanges instanceof KotlinSourceChanges.Known);
    assertTrue(
        ((KotlinSourceChanges.Known) sourceChanges)
            .getAddedAndModifiedFiles()
            .contains(Paths.get("b.kt")));
    assertTrue(((KotlinSourceChanges.Known) sourceChanges).getRemovedFiles().isEmpty());
  }

  @Test
  public void when_file_change_then_added_to_modified_files() {
    when(mockActionMetadata.getCurrentDigest())
        .thenReturn(
            new HashMap<Path, String>() {
              {
                put(Paths.get("A.jar"), "1");
                put(Paths.get("a.kt"), "1");
                put(Paths.get("B.jar"), "2");
                put(Paths.get("b.kt"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    when(mockActionMetadata.getPreviousDigest())
        .thenReturn(
            new HashMap<Path, String>() {
              {
                put(Paths.get("A.jar"), "1");
                put(Paths.get("a.kt"), "1");
                put(Paths.get("B.jar"), "2");
                put(Paths.get("b.kt"), "3");
                put(Paths.get("c.sh"), "3");
              }
            });

    KotlinSourceChanges sourceChanges = KotlinSourceChangesFactory.create(mockActionMetadata);

    assertTrue(sourceChanges instanceof KotlinSourceChanges.Known);
    assertTrue(
        ((KotlinSourceChanges.Known) sourceChanges)
            .getAddedAndModifiedFiles()
            .contains(Paths.get("b.kt")));
    assertTrue(((KotlinSourceChanges.Known) sourceChanges).getRemovedFiles().isEmpty());
  }

  @Test
  public void when_file_removal_then_added_to_removed_files() {
    when(mockActionMetadata.getCurrentDigest())
        .thenReturn(
            new HashMap<Path, String>() {
              {
                put(Paths.get("A.jar"), "1");
                put(Paths.get("a.kt"), "1");
                put(Paths.get("c.sh"), "3");
              }
            });
    when(mockActionMetadata.getPreviousDigest())
        .thenReturn(
            new HashMap<Path, String>() {
              {
                put(Paths.get("A.jar"), "1");
                put(Paths.get("a.kt"), "1");
                put(Paths.get("B.jar"), "2");
                put(Paths.get("b.kt"), "3");
                put(Paths.get("c.sh"), "3");
              }
            });

    KotlinSourceChanges sourceChanges = KotlinSourceChangesFactory.create(mockActionMetadata);

    assertTrue(sourceChanges instanceof KotlinSourceChanges.Known);
    assertTrue(
        ((KotlinSourceChanges.Known) sourceChanges).getRemovedFiles().contains(Paths.get("b.kt")));
    assertTrue(((KotlinSourceChanges.Known) sourceChanges).getAddedAndModifiedFiles().isEmpty());
  }
}
