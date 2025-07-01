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

import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.ClasspathChanges;
import com.google.common.collect.ImmutableList;
import java.nio.file.Paths;
import java.util.HashMap;
import org.junit.Before;
import org.junit.Test;

public class ClasspathChangesFactoryTest {

  private ActionMetadata mockActionMetadata;

  @Before
  public void setUp() {
    mockActionMetadata = mock(ActionMetadata.class);
  }

  @Test
  public void
      when_currentDigest_larger_then_previousDigest_then_ToBeComputedByIncrementalCompiler() {
    when(mockActionMetadata.getCurrentDigest())
        .thenReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "1");
                put(Paths.get("b.jar"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    when(mockActionMetadata.getPreviousDigest())
        .thenReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "1");
                put(Paths.get("b.class"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(mockActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void
      when_currentDigest_smaller_then_previousDigest_then_ToBeComputedByIncrementalCompiler() {
    when(mockActionMetadata.getCurrentDigest())
        .thenReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "1");
                put(Paths.get("b.class"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    when(mockActionMetadata.getPreviousDigest())
        .thenReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "1");
                put(Paths.get("b.jar"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(mockActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void when_currentDigest_changed_then_ToBeComputedByIncrementalCompiler() {
    when(mockActionMetadata.getCurrentDigest())
        .thenReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "4");
                put(Paths.get("b.jar"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    when(mockActionMetadata.getPreviousDigest())
        .thenReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "1");
                put(Paths.get("b.jar"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(mockActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void when_currentDigest_not_changed_then_NoChanges() {
    when(mockActionMetadata.getCurrentDigest())
        .thenReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "1");
                put(Paths.get("b.jar"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    when(mockActionMetadata.getPreviousDigest())
        .thenReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "1");
                put(Paths.get("b.jar"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(mockActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
  }

  @Test
  public void when_no_jar_NoChanges() {
    when(mockActionMetadata.getCurrentDigest())
        .thenReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.class"), "1");
                put(Paths.get("b.class"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    when(mockActionMetadata.getPreviousDigest())
        .thenReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.class"), "4");
                put(Paths.get("b.class"), "5");
                put(Paths.get("c.sh"), "6");
              }
            });

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(mockActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
  }
}
