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

import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams;
import com.facebook.buck.jvm.kotlin.buildtools.BuildToolsKotlinc;
import com.facebook.buck.jvm.kotlin.kotlinc.Kotlinc;
import org.junit.Before;
import org.junit.Test;

public class InMemoryKotlincFactoryTest {

  private KotlinExtraParams mockKotlinExtraParams;

  @Before
  public void setUp() {
    mockKotlinExtraParams = mock(KotlinExtraParams.class);
  }

  @Test
  public void when_shouldKotlincRunViaBuildToolsApi_then_BuildToolsKotlinc() {
    when(mockKotlinExtraParams.getShouldKotlincRunViaBuildToolsApi()).thenReturn(true);

    Kotlinc kotlinc = InMemoryKotlincFactory.create(mockKotlinExtraParams);

    assertTrue(kotlinc instanceof BuildToolsKotlinc);
  }

  @Test
  public void when_not_shouldKotlincRunViaBuildToolsApi_then_JarBackedReflectedKotlinc() {
    when(mockKotlinExtraParams.getShouldKotlincRunViaBuildToolsApi()).thenReturn(false);

    Kotlinc kotlinc = InMemoryKotlincFactory.create(mockKotlinExtraParams);

    assertTrue(kotlinc instanceof JarBackedReflectedKotlinc);
  }
}
