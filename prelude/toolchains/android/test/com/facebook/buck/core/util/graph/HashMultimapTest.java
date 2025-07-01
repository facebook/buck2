/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.core.util.graph;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.google.common.collect.HashMultimap;
import org.junit.Test;

public class HashMultimapTest {

  /**
   * The specification on {@link HashMultimap} is silent on whether {@link
   * HashMultimap#remove(Object, Object)} removes the key from the Multimap if the key/value pair is
   * the last entry for the key in the map. This test verifies this behavior as {@link
   * MutableDirectedGraph} depends on it.
   */
  @Test
  public void testRemoveLastEntryRemovesKey() {
    HashMultimap<String, String> edges = HashMultimap.create();
    edges.put("A", "B");
    edges.put("A", "C");
    edges.put("A", "D");
    assertEquals(3, edges.size());
    assertTrue(edges.containsKey("A"));

    edges.remove("A", "B");
    edges.remove("A", "C");
    edges.remove("A", "D");
    assertEquals(0, edges.size());
    assertFalse(edges.containsKey("A"));
  }
}
