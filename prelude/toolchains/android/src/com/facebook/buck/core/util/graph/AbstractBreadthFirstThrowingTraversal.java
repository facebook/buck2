/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.core.util.graph;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;

/**
 * Performs a breadth-first traversal of dependencies of a graph node.
 *
 * <p>In cases where the cost of allocating {@link Iterable}s for node dependencies is
 * expensive/costly, {@link ConsumingTraverser#breadthFirst(Iterable, ForEachSuccessorFunction)}
 * provides a lighter-weight option.
 *
 * <p>TODO: Implement via {@link ConsumingTraverser}.
 */
public abstract class AbstractBreadthFirstThrowingTraversal<Node, E extends Throwable> {

  private final Queue<Node> toExplore;
  private final Set<Node> explored;

  public AbstractBreadthFirstThrowingTraversal(Node initialNode) {
    this(ImmutableSet.of(initialNode));
  }

  public AbstractBreadthFirstThrowingTraversal(Iterable<? extends Node> initialNodes) {
    toExplore = new LinkedList<>();
    Iterables.addAll(toExplore, initialNodes);
    explored = new HashSet<>();
  }

  public final void start() throws E {
    while (!toExplore.isEmpty()) {
      Node currentNode = toExplore.remove();
      if (explored.contains(currentNode)) {
        continue;
      }

      Iterable<? extends Node> depsToVisit = this.visit(currentNode);
      explored.add(currentNode);

      for (Node dep : depsToVisit) {
        if (!explored.contains(dep)) {
          toExplore.add(dep);
        }
      }
    }

    this.onComplete();
  }

  /** Override this method with any logic that should be run when {@link #start()} completes. */
  protected void onComplete() throws E {}

  /**
   * To perform a full traversal of the the {@code initialNode}'s transitive dependencies, this
   * function should return all of {@code node}'s direct dependencies.
   *
   * @param node Visited graph node
   * @return The set of direct dependencies to visit after visiting this node.
   */
  public abstract Iterable<? extends Node> visit(Node node) throws E;

  /**
   * This will typically be implemented as a lambda passed to {@link #traverse(Object, Visitor)} or
   * {@link #traverse(Iterable, Visitor)}
   */
  public interface Visitor<Node, E extends Throwable> {
    Iterable<Node> visit(Node node) throws E;
  }

  protected static class StaticBreadthFirstTraversal<Node>
      extends AbstractBreadthFirstThrowingTraversal<Node, RuntimeException> {

    private final Visitor<Node, RuntimeException> visitor;

    protected StaticBreadthFirstTraversal(
        Node initialNode, Visitor<Node, RuntimeException> visitor) {
      super(initialNode);
      this.visitor = visitor;
    }

    protected StaticBreadthFirstTraversal(
        Iterable<? extends Node> initialNodes, Visitor<Node, RuntimeException> visitor) {
      super(initialNodes);
      this.visitor = visitor;
    }

    @Override
    public Iterable<Node> visit(Node node) throws RuntimeException {
      return visitor.visit(node);
    }
  }

  /**
   * Traverse a graph without explicitly creating a {@code new
   * AbstractBreadthFirstThrowingTraversal} and overriding {@link #visit(Object)}
   *
   * @param visitor Typically a lambda expression
   */
  public static <Node> void traverse(Node initialNode, Visitor<Node, RuntimeException> visitor) {
    new StaticBreadthFirstTraversal<>(initialNode, visitor).start();
  }

  /**
   * Traverse a graph without explicitly creating a {@code new
   * AbstractBreadthFirstThrowingTraversal} and overriding {@link #visit(Object)}
   *
   * @param visitor Typically a lambda expression
   */
  public static <Node> void traverse(
      Iterable<? extends Node> initialNodes, Visitor<Node, RuntimeException> visitor) {
    new StaticBreadthFirstTraversal<>(initialNodes, visitor).start();
  }
}
