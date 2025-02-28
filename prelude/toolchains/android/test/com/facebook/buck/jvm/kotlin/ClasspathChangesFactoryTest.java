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

import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.ClasspathChanges;
import com.google.common.collect.ImmutableList;
import java.nio.file.Paths;
import java.util.HashMap;
import org.easymock.EasyMock;
import org.junit.Before;
import org.junit.Test;

public class ClasspathChangesFactoryTest {

  private ActionMetadata mockActionMetadata;

  @Before
  public void setUp() {
    mockActionMetadata = EasyMock.createMock(ActionMetadata.class);
  }

  @Test
  public void
      when_currentDigest_larger_then_previousDigest_then_ToBeComputedByIncrementalCompiler() {
    expect(mockActionMetadata.getCurrentDigest())
        .andReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "1");
                put(Paths.get("b.jar"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    expect(mockActionMetadata.getPreviousDigest())
        .andReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "1");
                put(Paths.get("b.class"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    replay(mockActionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(mockActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void
      when_currentDigest_smaller_then_previousDigest_then_ToBeComputedByIncrementalCompiler() {
    expect(mockActionMetadata.getCurrentDigest())
        .andReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "1");
                put(Paths.get("b.class"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    expect(mockActionMetadata.getPreviousDigest())
        .andReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "1");
                put(Paths.get("b.jar"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    replay(mockActionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(mockActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void when_currentDigest_changed_then_ToBeComputedByIncrementalCompiler() {
    expect(mockActionMetadata.getCurrentDigest())
        .andReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "4");
                put(Paths.get("b.jar"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    expect(mockActionMetadata.getPreviousDigest())
        .andReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "1");
                put(Paths.get("b.jar"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    replay(mockActionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(mockActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void when_currentDigest_not_changed_then_NoChanges() {
    expect(mockActionMetadata.getCurrentDigest())
        .andReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "1");
                put(Paths.get("b.jar"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    expect(mockActionMetadata.getPreviousDigest())
        .andReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.jar"), "1");
                put(Paths.get("b.jar"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    replay(mockActionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(mockActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
  }

  @Test
  public void when_no_jar_NoChanges() {
    expect(mockActionMetadata.getCurrentDigest())
        .andReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.class"), "1");
                put(Paths.get("b.class"), "2");
                put(Paths.get("c.sh"), "3");
              }
            });
    expect(mockActionMetadata.getPreviousDigest())
        .andReturn(
            new HashMap<>() {
              {
                put(Paths.get("a.class"), "4");
                put(Paths.get("b.class"), "5");
                put(Paths.get("c.sh"), "6");
              }
            });
    replay(mockActionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(mockActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
  }
}
