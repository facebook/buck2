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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.emptyIterable;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.google.common.collect.ImmutableSet;
import java.util.Set;
import org.junit.Test;

public class MutableDirectedGraphTest {

  @Test
  public void testCreateNewGraph() {
    MutableDirectedGraph<String> graph = new MutableDirectedGraph<>();
    assertEquals(0, graph.getNodeCount());
    assertEquals(0, graph.getEdgeCount());
    assertFalse(graph.containsNode("A"));
    assertFalse(graph.containsEdge("A", "B"));
    assertTrue(graph.isAcyclic());
    assertEquals(ImmutableSet.<String>of(), graph.getNodes());
  }

  @Test
  public void testCreateGraphWithOneEdge() {
    MutableDirectedGraph<String> graph = new MutableDirectedGraph<>();
    Set<String> nodesView = graph.getNodes();
    assertEquals(ImmutableSet.<String>of(), nodesView);
    graph.addEdge("A", "B");
    assertEquals(2, graph.getNodeCount());
    assertEquals(1, graph.getEdgeCount());
    assertTrue(graph.containsEdge("A", "B"));
    assertEquals(ImmutableSet.of("A"), graph.getNodesWithNoIncomingEdges());
    assertTrue(graph.isAcyclic());
    assertEquals(ImmutableSet.of("A", "B"), nodesView);
  }

  /**
   * Make sure a node can point to itself. Also ensure that when a self-referential edge is removed,
   * the graph is left in the correct state.
   */
  @Test
  public void testAddSelfReferentialEdge() {
    MutableDirectedGraph<String> graph = new MutableDirectedGraph<>();
    graph.addEdge("A", "A");
    assertEquals(1, graph.getNodeCount());
    assertEquals(1, graph.getEdgeCount());
    assertTrue(graph.containsNode("A"));
    assertTrue(graph.containsEdge("A", "A"));
    assertThat(graph.getOutgoingNodesFor("A"), containsInAnyOrder("A"));
    assertThat(graph.getIncomingNodesFor("A"), containsInAnyOrder("A"));
    assertTrue(graph.hasIncomingEdges("A"));
    assertFalse(graph.isAcyclic());
    assertEquals(ImmutableSet.of(ImmutableSet.of("A")), graph.findCycles());

    graph.removeNode("A");
    assertEquals(0, graph.getNodeCount());
    assertEquals(0, graph.getEdgeCount());
    assertFalse(graph.containsNode("A"));
    assertFalse(graph.containsEdge("A", "A"));
    assertThat(graph.getOutgoingNodesFor("A"), emptyIterable());
    assertThat(graph.getIncomingNodesFor("A"), emptyIterable());
    assertTrue(graph.isAcyclic());
  }

  @Test
  public void testIsAcyclic() {
    MutableDirectedGraph<String> graph = new MutableDirectedGraph<>();
    graph.addEdge("A", "B");
    assertTrue(graph.isAcyclic());
    graph.addEdge("B", "C");
    assertTrue(graph.isAcyclic());
    graph.addEdge("C", "A");
    graph.addEdge("D", "E");
    assertFalse("Graph has a cycle: A->B->C", graph.isAcyclic());
    assertEquals(ImmutableSet.of(ImmutableSet.of("A", "B", "C")), graph.findCycles());

    graph.removeEdge("C", "A");
    assertTrue(graph.isAcyclic());
    graph.addEdge("A", "D");
    assertTrue(graph.isAcyclic());
    graph.addEdge("D", "C");
    assertTrue(graph.isAcyclic());
  }

  @Test
  public void testFindCycles() {
    MutableDirectedGraph<String> graph = new MutableDirectedGraph<>();
    graph.addEdge("A", "B");
    graph.addEdge("B", "C");
    graph.addEdge("C", "A");
    graph.addEdge("D", "E");
    graph.addEdge("E", "D");
    graph.addEdge("F", "F");
    assertEquals(
        ImmutableSet.of(
            ImmutableSet.of("A", "B", "C"), ImmutableSet.of("D", "E"), ImmutableSet.of("F")),
        graph.findCycles());
  }

  @Test
  public void testTrivialDisconnectedGraphIsAcyclic() {
    MutableDirectedGraph<String> graph = new MutableDirectedGraph<>();
    graph.addNode("A");
    graph.addNode("B");
    graph.addNode("C");
    assertTrue(graph.isAcyclic());
  }
}
