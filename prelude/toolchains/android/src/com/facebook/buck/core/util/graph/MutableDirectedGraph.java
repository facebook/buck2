/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.core.util.graph;

import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

/**
 * Represents a directed graph with unweighted edges. For a given source and sink node pair, there
 * is at most one directed edge connecting them in the graph. The graph is not required to be
 * connected or acyclic.
 *
 * @param <T> the type of object stored as nodes in this graph
 */
public final class MutableDirectedGraph<T> implements TraversableGraph<T> {

  /**
   * It is possible to have a node in the graph without any edges, which is why we must maintain a
   * separate collection for nodes in the graph, rather than just using the keySet of {@link
   * #outgoingEdges}.
   */
  private final Set<T> nodes;

  /**
   * Represents the edges in the graph. Keys are source nodes; values are corresponding sync nodes.
   */
  private final Map<T, Collection<T>> outgoingEdges;

  /**
   * Represents the edges in the graph. Keys are sink nodes; values are corresponding source nodes.
   */
  private final Map<T, Collection<T>> incomingEdges;

  /** Creates a new edge list. Used when a edge is added for a node for the first time. */
  private final Supplier<Collection<T>> createEdgeCollection;

  private MutableDirectedGraph(
      Set<T> nodes,
      Map<T, Collection<T>> outgoingEdges,
      Map<T, Collection<T>> incomingEdges,
      Supplier<Collection<T>> createEdgeCollection) {
    this.nodes = nodes;
    this.outgoingEdges = outgoingEdges;
    this.incomingEdges = incomingEdges;
    this.createEdgeCollection = createEdgeCollection;
  }

  /** Creates a new graph with no nodes or edges. */
  public MutableDirectedGraph() {
    this(new LinkedHashSet<>(), new HashMap<>(), new HashMap<>(), LinkedHashSet::new);
  }

  public static <T> MutableDirectedGraph<T> createConcurrent() {
    return new MutableDirectedGraph<>(
        Collections.newSetFromMap(new ConcurrentHashMap<>()),
        new ConcurrentHashMap<>(),
        new ConcurrentHashMap<>(),
        () -> Collections.synchronizedSet(new HashSet<>()));
  }

  /**
   * @return the number of nodes in the graph
   */
  public int getNodeCount() {
    return nodes.size();
  }

  /**
   * @return the number of edges in the graph
   */
  public int getEdgeCount() {
    return outgoingEdges.size();
  }

  /**
   * @return whether the specified node is present in the graph
   */
  public boolean containsNode(T node) {
    return nodes.contains(node);
  }

  /**
   * @return whether an edge from the source to the sink is present in the graph
   */
  public boolean containsEdge(T source, T sink) {
    return outgoingEdges.getOrDefault(source, ImmutableSet.of()).contains(sink);
  }

  /** Adds the specified node to the graph. */
  public boolean addNode(T node) {
    return nodes.add(node);
  }

  /** Removes the specified node from the graph. */
  public boolean removeNode(T node) {
    boolean isRemoved = nodes.remove(node);
    Collection<T> nodesReachableFromTheSpecifiedNode = outgoingEdges.remove(node);
    if (nodesReachableFromTheSpecifiedNode != null) {
      for (T reachableNode : nodesReachableFromTheSpecifiedNode) {
        incomingEdges.get(reachableNode).remove(node);
      }
    }
    return isRemoved;
  }

  /**
   * Adds an edge between {@code source} and {@code sink}. Adds the nodes to the graph if they are
   * not already present.
   */
  public void addEdge(T source, T sink) {
    nodes.add(source);
    nodes.add(sink);
    outgoingEdges.computeIfAbsent(source, s -> createEdgeCollection.get()).add(sink);
    incomingEdges.computeIfAbsent(sink, s -> createEdgeCollection.get()).add(source);
  }

  /**
   * Sets all outgoing edges between {@code source} and {@code sinks} to the given collections. Note
   * that the given collection is used as-is -- the caller must make sure it can be used safely by
   * this graph (e.g. concurrent modifications when used with a concurrent builder).
   */
  public void setEdges(T source, Collection<T> sinks) {
    nodes.add(source);
    nodes.addAll(sinks);
    Preconditions.checkState(outgoingEdges.putIfAbsent(source, sinks) == null);
    sinks.forEach(
        sink -> incomingEdges.computeIfAbsent(sink, s -> createEdgeCollection.get()).add(source));
  }

  /**
   * Removes the edge between {@code source} and {@code sink}. This does not remove {@code source}
   * or {@code sink} from the graph. Note that this may leave either {@code source} or {@code sink}
   * as unconnected nodes in the graph.
   */
  public void removeEdge(T source, T sink) {
    outgoingEdges.get(source).remove(sink);
    incomingEdges.get(sink).remove(source);
  }

  @Override
  public Iterable<T> getOutgoingNodesFor(T source) {
    return Collections.unmodifiableCollection(
        outgoingEdges.getOrDefault(source, ImmutableList.of()));
  }

  @Override
  public Iterable<T> getIncomingNodesFor(T sink) {
    return Collections.unmodifiableCollection(incomingEdges.getOrDefault(sink, ImmutableList.of()));
  }

  public boolean hasIncomingEdges(T node) {
    return this.incomingEdges.containsKey(node);
  }

  @Override
  public Set<T> getNodes() {
    return Collections.unmodifiableSet(nodes);
  }

  public boolean isAcyclic() {
    return TraversableGraphs.isAcyclic(nodes, this::getOutgoingNodesFor);
  }

  public ImmutableSet<ImmutableSet<T>> findCycles() {
    ImmutableSet.Builder<ImmutableSet<T>> builder = ImmutableSet.builder();
    TraversableGraphs.findStronglyConnectedComponents(this).stream()
        .filter(s -> s.size() > 1)
        .forEach(builder::add);
    // Tarjan's algorithm (as pseudo-coded on Wikipedia) does not appear to account for single-node
    // cycles. Therefore, we must check for them exclusively.
    for (T node : nodes) {
      if (containsEdge(node, node)) {
        builder.add(ImmutableSet.of(node));
      }
    }
    return builder.build();
  }

  @Override
  public Iterable<T> getNodesWithNoIncomingEdges() {
    return Sets.difference(nodes, incomingEdges.keySet());
  }

  @Override
  public Iterable<T> getNodesWithNoOutgoingEdges() {
    return Sets.difference(nodes, outgoingEdges.keySet());
  }

  ImmutableSet<T> createImmutableCopyOfNodes() {
    return ImmutableSet.copyOf(nodes);
  }

  ImmutableMap<T, ImmutableSet<T>> createImmutableCopyOfOutgoingEdges() {
    ImmutableMap.Builder<T, ImmutableSet<T>> builder =
        ImmutableMap.builderWithExpectedSize(outgoingEdges.size());
    outgoingEdges.forEach(
        (n, e) ->
            builder.put(n, ImmutableSet.<T>builderWithExpectedSize(e.size()).addAll(e).build()));
    return builder.build();
  }

  ImmutableMap<T, ImmutableSet<T>> createImmutableCopyOfIncomingEdges() {
    ImmutableMap.Builder<T, ImmutableSet<T>> builder =
        ImmutableMap.builderWithExpectedSize(incomingEdges.size());
    incomingEdges.forEach(
        (n, e) ->
            builder.put(n, ImmutableSet.<T>builderWithExpectedSize(e.size()).addAll(e).build()));
    return builder.build();
  }
}
