/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.core.util.graph;

import static org.junit.Assert.assertEquals;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSortedSet;
import com.google.common.collect.Iterables;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

public class AbstractBreadthFirstTraversalTest {

  @Test
  public void testIsBreadthFirst() {
    // The dependency graph is built as follows:
    //
    //                       J
    //                /    /   \     \
    //              F    G       H     I
    //            /   \        /   \     \
    //          E       D    C       B     A
    //
    FakeNode nodeA = createNode("A");
    FakeNode nodeB = createNode("B");
    FakeNode nodeC = createNode("C");
    FakeNode nodeD = createNode("D");
    FakeNode nodeE = createNode("E");

    FakeNode nodeF = createNode("F", nodeE, nodeD);
    FakeNode nodeG = createNode("G");
    FakeNode nodeH = createNode("H", nodeC, nodeB);
    FakeNode nodeI = createNode("I", nodeA);

    FakeNode initialNode = createNode("J", nodeF, nodeG, nodeH, nodeI);

    List<FakeNode> nodeTraversalOrder = new ArrayList<>();
    new AbstractBreadthFirstTraversal<FakeNode>(initialNode) {
      @Override
      public ImmutableSet<FakeNode> visit(FakeNode node) {
        nodeTraversalOrder.add(node);
        return node.getDeps();
      }
    }.start();

    assertEquals(
        "Dependencies should be explored breadth-first, using lexicographic ordering to break ties",
        ImmutableList.of(
            initialNode, nodeF, nodeG, nodeH, nodeI, nodeD, nodeE, nodeB, nodeC, nodeA),
        nodeTraversalOrder);
  }

  @Test
  public void testSubsetWorks() {
    // The dependency graph is built as follows:
    //
    //                       10
    //                /    /   \     \
    //              6    7       8     9
    //            /   \        /   \     \
    //          5       4    3       2     1
    //
    FakeNode node1 = createNode("1");
    FakeNode node2 = createNode("2");
    FakeNode node3 = createNode("3");
    FakeNode node4 = createNode("4");
    FakeNode node5 = createNode("5");

    FakeNode node6 = createNode("6", node5, node4);
    FakeNode node7 = createNode("7");
    FakeNode node8 = createNode("8", node3, node2);
    FakeNode node9 = createNode("9", node1);

    FakeNode initialNode = createNode("10", node6, node7, node8, node9);

    List<FakeNode> nodeTraversalOrder = new ArrayList<>();

    // This visitor only visits dependencies whose node names are even numbers.
    new AbstractBreadthFirstTraversal<FakeNode>(initialNode) {
      @Override
      public Iterable<FakeNode> visit(FakeNode node) {
        nodeTraversalOrder.add(node);
        return ImmutableSet.copyOf(
            Iterables.filter(node.getDeps(), input -> Integer.parseInt(input.getName()) % 2 == 0));
      }
    }.start();

    assertEquals(
        "Dependencies should be explored breadth-first, only containing nodes whose node name is "
            + "an even number",
        ImmutableList.of(initialNode, node6, node8, node4, node2),
        nodeTraversalOrder);
  }

  private static class FakeNode implements Comparable<FakeNode> {
    protected final String name;
    protected final ImmutableSortedSet<FakeNode> deps;

    public FakeNode(String name, ImmutableSortedSet<FakeNode> deps) {
      this.name = name;
      this.deps = deps;
    }

    public String getName() {
      return name;
    }

    public ImmutableSet<FakeNode> getDeps() {
      return deps;
    }

    @Override
    public int compareTo(FakeNode other) {
      if (this == other) {
        return 0;
      }

      return this.name.compareTo(other.name);
    }
  }

  private FakeNode createNode(String name, FakeNode... deps) {
    return new FakeNode(name, ImmutableSortedSet.copyOf(deps));
  }
}
