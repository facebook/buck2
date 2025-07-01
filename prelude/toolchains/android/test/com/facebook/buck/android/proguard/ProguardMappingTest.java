/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.proguard;

import static org.junit.Assert.assertEquals;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import java.util.Map;
import org.junit.Test;

public class ProguardMappingTest {
  @Test
  public void testBasicParse() {
    Map<String, String> mapping =
        ProguardMapping.readClassMapping(
            ImmutableList.of(
                "foo.bar.Baz -> foo.bar.a:",
                "  member -> x",
                "foo.bar.Baz$Qux -> foo.bar.Baz$Qux:"));
    assertEquals(
        mapping,
        ImmutableMap.of(
            "foo.bar.Baz", "foo.bar.a",
            "foo.bar.Baz$Qux", "foo.bar.Baz$Qux"));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testInternalNameError() {
    ProguardMapping.readClassMapping(ImmutableList.of("foo/bar/Baz -> foo/bar/a:"));
  }
}
