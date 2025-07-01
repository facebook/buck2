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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class TraversableGraphsTest {

  @Test
  public void isAcyclicSelfEdge() {
    MutableDirectedGraph<String> graph = new MutableDirectedGraph<>();
    graph.addEdge("A", "B");
    graph.addEdge("B", "C");
    graph.addEdge("C", "D");
    graph.addEdge("D", "D");
    assertFalse(TraversableGraphs.isAcyclic(graph.getNodes(), graph::getOutgoingNodesFor));
    assertFalse(
        TraversableGraphs.isAcyclic(
            graph.getNodesWithNoIncomingEdges(), graph::getOutgoingNodesFor));
  }

  @Test
  public void isAcyclicNoCycles() {
    MutableDirectedGraph<String> graph = new MutableDirectedGraph<>();
    graph.addEdge("A", "B");
    graph.addEdge("A", "D");
    graph.addEdge("B", "C");
    graph.addEdge("B", "D");
    graph.addEdge("C", "D");
    assertTrue(TraversableGraphs.isAcyclic(graph.getNodes(), graph::getOutgoingNodesFor));
    assertTrue(
        TraversableGraphs.isAcyclic(
            graph.getNodesWithNoIncomingEdges(), graph::getOutgoingNodesFor));
  }

  @Test
  public void isAcyclicNoCyclesDiamond() {
    MutableDirectedGraph<String> graph = new MutableDirectedGraph<>();
    graph.addEdge("A", "B");
    graph.addEdge("A", "C");
    graph.addEdge("A", "D");
    graph.addEdge("A", "E");
    graph.addEdge("E", "B");
    assertTrue(TraversableGraphs.isAcyclic(graph.getNodes(), graph::getOutgoingNodesFor));
    assertTrue(
        TraversableGraphs.isAcyclic(
            graph.getNodesWithNoIncomingEdges(), graph::getOutgoingNodesFor));
  }

  @Test
  public void isAcyclicCycle() {
    MutableDirectedGraph<String> graph = new MutableDirectedGraph<>();
    graph.addEdge("A", "B");
    graph.addEdge("B", "C");
    graph.addEdge("C", "D");
    graph.addEdge("D", "A");
    assertFalse(TraversableGraphs.isAcyclic(graph.getNodes(), graph::getOutgoingNodesFor));
  }
}
