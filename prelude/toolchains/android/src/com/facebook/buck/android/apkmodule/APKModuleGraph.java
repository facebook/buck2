/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.android.apkmodule;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.util.graph.AbstractBreadthFirstTraversal;
import com.facebook.buck.core.util.graph.DirectedAcyclicGraph;
import com.facebook.buck.core.util.graph.MutableDirectedGraph;
import com.facebook.buck.io.file.PathMatcher;
import com.facebook.buck.jvm.java.classes.ClasspathTraversal;
import com.facebook.buck.jvm.java.classes.ClasspathTraverser;
import com.facebook.buck.jvm.java.classes.DefaultClasspathTraverser;
import com.facebook.buck.jvm.java.classes.FileLike;
import com.facebook.buck.util.MoreSuppliers;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableMap.Builder;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSetMultimap;
import com.google.common.collect.ImmutableSortedMap;
import com.google.common.collect.ImmutableSortedSet;
import com.google.common.collect.Multimap;
import com.google.common.collect.MultimapBuilder;
import com.google.common.collect.Ordering;
import com.google.common.collect.Sets;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Utility class for grouping sets of targets and their dependencies into APK Modules containing
 * their exclusive dependencies. Targets that are dependencies of the root target are included in
 * the root. Targets that have dependencies in two are more groups are put the APKModule that
 * represents the dependent modules minimal cover based on the declared dependencies given. If the
 * minimal cover contains more than one APKModule, the target will belong to a new shared APKModule
 * that is a dependency of all APKModules in the minimal cover.
 */
public class APKModuleGraph<BuildTargetType extends Comparable<BuildTargetType>> {

  private final TargetGraphInterface<BuildTargetType> targetGraph;
  private final BuildTargetType target;

  private final Optional<ImmutableMap<String, ImmutableList<BuildTargetType>>>
      suppliedSeedConfigMap;

  private final Optional<ImmutableMap<String, ImmutableList<String>>> appModuleDependencies;

  private final Optional<List<BuildTargetType>> denylistModules;

  /**
   * This is the computed in the constructor - this is the global view of how every module is laid
   * out
   */
  private final ImmutableSortedMap<APKModule, ImmutableSet<BuildTargetType>> moduleToTarget;

  private final Set<UndeclaredDependency> undeclaredDependencies = new HashSet<>();

  /** This is the set of targets reachable by the root that does NOT pass through a seed. */
  private final Supplier<ImmutableSet<BuildTargetType>> rootTargetsSupplier =
      MoreSuppliers.memoize(this::generateRootTargets);

  private final Supplier<ImmutableMap<BuildTargetType, APKModule>> targetToModuleMapSupplier =
      MoreSuppliers.memoize(
          () -> {
            Builder<BuildTargetType, APKModule> mapBuilder = ImmutableMap.builder();
            new AbstractBreadthFirstTraversal<APKModule>(getGraph().getNodesWithNoIncomingEdges()) {
              @Override
              public ImmutableSet<APKModule> visit(APKModule node) {
                if (node.equals(getRootAPKModule())) {
                  return ImmutableSet.of();
                }
                getBuildTargets(node).forEach(input -> mapBuilder.put(input, node));
                return getGraph().getOutgoingNodesFor(node);
              }
            }.start();
            return mapBuilder.build();
          });

  private APKModule rootAPKModule = new APKModule(APKModule.ROOT_APKMODULE_NAME);

  private final DirectedAcyclicGraph<APKModule> graph;

  private final Supplier<DirectedAcyclicGraph<String>> declaredDependencyGraphSupplier =
      MoreSuppliers.memoize(this::generateDeclaredDependencyGraph);

  private final Supplier<DirectedAcyclicGraph<String>> detectedDepAndDeclaredDepGraphSupplier =
      MoreSuppliers.memoize(this::generateDetectedDependencyAndDeclaredDependencyGraph);

  private final Supplier<ImmutableSet<APKModule>> modulesSupplier =
      MoreSuppliers.memoize(
          () -> {
            ImmutableSet.Builder<APKModule> moduleBuilder = ImmutableSet.builder();
            new AbstractBreadthFirstTraversal<APKModule>(getRootAPKModule()) {
              @Override
              public Iterable<APKModule> visit(APKModule apkModule) throws RuntimeException {
                moduleBuilder.add(apkModule);
                return getGraph().getIncomingNodesFor(apkModule);
              }
            }.start();
            return moduleBuilder.build();
          });

  private final Supplier<ImmutableMultimap<BuildTargetType, String>> sharedSeedsSupplier =
      MoreSuppliers.memoize(this::generateSharedSeeds);

  private final Supplier<Optional<ImmutableMap<String, ImmutableList<BuildTargetType>>>>
      configMapSupplier = MoreSuppliers.memoize(this::generateSeedConfigMap);

  private final Supplier<Optional<ImmutableMap<BuildTargetType, String>>> seedTargetMapSupplier =
      MoreSuppliers.memoize(this::generateSeedTargetMap);

  /**
   * Constructor for the {@code APKModule} graph generator object that produces a graph with only a
   * root module.
   */
  public APKModuleGraph(TargetGraphInterface<BuildTargetType> targetGraph, BuildTargetType target) {
    this(targetGraph, target, Optional.empty(), Optional.empty(), Optional.empty());
  }

  /**
   * Constructor for the {@code APKModule} graph generator object
   *
   * @param targetGraph The full target graph of the build
   * @param target The root target to use to traverse the graph
   * @param suppliedSeedConfigMap A map of names to seed targets to use for creating {@code
   *     APKModule}.
   * @param appModuleDependencies
   *     <p>a mapping of declared dependencies between module names. If a APKModule <b>m1</b>
   *     depends on <b>m2</b>, it implies to buck that in order for <b>m1</b> to be available for
   *     execution <b>m2</b> must be available for use as well. Because of this, we can say that
   *     including a buck target in <b>m2</b> effectively includes the buck-target in <b>m2's</b>
   *     dependent <b>m1</b>. In other words, <b>m2</b> covers <b>m1</b>. Therefore, if a buck
   *     target is required by both these modules, we can safely place it in the minimal cover which
   *     is the APKModule <b>m2</b>.
   * @param denylistModules A list of targets that will NOT be included in any module.
   */
  public APKModuleGraph(
      TargetGraphInterface<BuildTargetType> targetGraph,
      BuildTargetType target,
      Optional<ImmutableMap<String, ImmutableList<BuildTargetType>>> suppliedSeedConfigMap,
      Optional<ImmutableMap<String, ImmutableList<String>>> appModuleDependencies,
      Optional<List<BuildTargetType>> denylistModules) {
    this.targetGraph = targetGraph;
    this.target = target;
    this.suppliedSeedConfigMap = suppliedSeedConfigMap;
    this.appModuleDependencies = appModuleDependencies;
    this.denylistModules = denylistModules;

    Map<APKModule, ImmutableSet.Builder<BuildTargetType>> builder = new HashMap<>();
    graph = generateGraph(builder);

    ImmutableSortedMap.Builder<APKModule, ImmutableSet<BuildTargetType>> mapBuilder =
        ImmutableSortedMap.naturalOrder();
    mapBuilder.put(getRootAPKModule(), ImmutableSortedSet.copyOf(rootTargetsSupplier.get()));
    for (Map.Entry<APKModule, ImmutableSet.Builder<BuildTargetType>> entry : builder.entrySet()) {
      mapBuilder.put(entry.getKey(), ImmutableSortedSet.copyOf(entry.getValue().build()));
    }
    moduleToTarget = mapBuilder.build();
  }

  public ImmutableSortedMap<APKModule, ImmutableSortedSet<APKModule>> toOutgoingEdgesMap() {
    return getAPKModules().stream()
        .collect(
            ImmutableSortedMap.toImmutableSortedMap(
                Ordering.natural(),
                module -> module,
                module -> ImmutableSortedSet.copyOf(getGraph().getOutgoingNodesFor(module))));
  }

  private Optional<ImmutableMap<String, ImmutableList<BuildTargetType>>> generateSeedConfigMap() {
    return suppliedSeedConfigMap;
  }

  private Optional<ImmutableMap<BuildTargetType, String>> generateSeedTargetMap() {
    if (suppliedSeedConfigMap.isPresent()) {
      final Map<BuildTargetType, String> seedTargetMap = new HashMap<>();
      for (final ImmutableMap.Entry<String, ImmutableList<BuildTargetType>> suppliedSeedConfig :
          suppliedSeedConfigMap.get().entrySet()) {
        for (final BuildTargetType seedTarget : suppliedSeedConfig.getValue()) {
          seedTargetMap.put(seedTarget, suppliedSeedConfig.getKey());
        }
      }
      return Optional.of(ImmutableMap.copyOf(seedTargetMap));
    }

    return Optional.empty();
  }

  /**
   * Lazy generate the graph on first use
   *
   * @return the DAG representing APKModules and their dependency relationships
   */
  public DirectedAcyclicGraph<APKModule> getGraph() {
    return graph;
  }

  /**
   * Lazy generate the declared dependency graph.
   *
   * @return the DAG representing the declared dependency relationship of declared app module
   *     configurations.
   */
  private DirectedAcyclicGraph<String> getDeclaredDependencyGraph() {
    return declaredDependencyGraphSupplier.get();
  }

  /**
   * Lazy generate the detected dep and declared dependency graph. Undeclared dependencies are
   * detected and automatically added to this DAG.
   *
   * @return the DAG representing the declared dependency relationship of declared app module
   *     configurations.
   */
  private DirectedAcyclicGraph<String> getDetectedDepAndDeclaredDepGraph() {
    return detectedDepAndDeclaredDepGraphSupplier.get();
  }

  /**
   * Get the APKModule representing the core application that is always included in the apk
   *
   * @return the root APK Module
   */
  public APKModule getRootAPKModule() {
    return rootAPKModule;
  }

  public ImmutableSet<APKModule> getAPKModules() {
    return modulesSupplier.get();
  }

  public Optional<ImmutableMap<String, ImmutableList<BuildTargetType>>> getSeedConfigMap() {
    verifyNoSharedSeeds();
    return configMapSupplier.get();
  }

  public Optional<ImmutableMap<BuildTargetType, String>> getSeedTargetMap() {
    return seedTargetMapSupplier.get();
  }

  /**
   * Get the Module that contains the given target
   *
   * @param target target to serach for in modules
   * @return the module that contains the target
   */
  public APKModule findModuleForTarget(BuildTargetType target) {
    APKModule module = targetToModuleMapSupplier.get().get(target);
    return (module == null ? getRootAPKModule() : module);
  }

  public Optional<List<BuildTargetType>> getDenyListModules() {
    return denylistModules;
  }

  /**
   * Group the classes in the input jars into a multimap based on the APKModule they belong to
   *
   * @param apkModuleToJarPathMap the mapping of APKModules to the path for the jar files
   * @param translatorFunction function used to translate the class names to obfuscated names
   * @return The mapping of APKModules to the class names they contain
   * @throws IOException
   */
  public static ImmutableMultimap<APKModule, String> getAPKModuleToClassesMap(
      ImmutableMultimap<APKModule, Path> apkModuleToJarPathMap,
      Function<String, String> translatorFunction,
      AbsPath rootPath,
      ImmutableSet<PathMatcher> ignoredPaths)
      throws IOException {
    ImmutableMultimap.Builder<APKModule, String> builder = ImmutableSetMultimap.builder();
    if (!apkModuleToJarPathMap.isEmpty()) {
      for (APKModule dexStore : apkModuleToJarPathMap.keySet()) {
        for (Path jarFilePath : apkModuleToJarPathMap.get(dexStore)) {
          ClasspathTraverser classpathTraverser = new DefaultClasspathTraverser();
          classpathTraverser.traverse(
              new ClasspathTraversal(ImmutableSet.of(jarFilePath), rootPath, ignoredPaths) {
                @Override
                public void visit(FileLike entry) {
                  if (!entry.getRelativePath().endsWith(".class")) {
                    // ignore everything but class files in the jar.
                    return;
                  }

                  String classpath = entry.getRelativePath().replaceAll("\\.class$", "");

                  if (translatorFunction.apply(classpath) != null) {
                    builder.put(dexStore, translatorFunction.apply(classpath));
                  }
                }
              });
        }
      }
    }
    return builder.build();
  }

  /**
   * Generate the graph by identifying root targets, then marking targets with the seeds they are
   * reachable with, then consolidating the targets reachable by multiple seeds into shared modules
   *
   * @return The graph of APKModules with edges representing dependencies between modules
   */
  private DirectedAcyclicGraph<APKModule> generateGraph(
      Map<APKModule, ImmutableSet.Builder<BuildTargetType>> builder) {
    MutableDirectedGraph<APKModule> apkModuleGraph = new MutableDirectedGraph<>();

    apkModuleGraph.addNode(getRootAPKModule());

    if (getSeedConfigMap().isPresent()) {
      Multimap<BuildTargetType, String> targetToContainingApkModulesMap =
          mapTargetsToContainingModules();
      generateSharedModules(apkModuleGraph, targetToContainingApkModulesMap, builder);
      // add declared dependencies as well.
      Map<String, APKModule> nameToAPKModules = new HashMap<>();
      for (APKModule node : apkModuleGraph.getNodes()) {
        nameToAPKModules.put(node.getName(), node);
      }
      final DirectedAcyclicGraph<String> detectedDepAndDeclaredDepGraph =
          getDetectedDepAndDeclaredDepGraph();
      for (String source : detectedDepAndDeclaredDepGraph.getNodes()) {
        for (String sink : detectedDepAndDeclaredDepGraph.getOutgoingNodesFor(source)) {
          apkModuleGraph.addEdge(nameToAPKModules.get(source), nameToAPKModules.get(sink));
        }
      }
    }

    return new DirectedAcyclicGraph<>(apkModuleGraph);
  }

  private DirectedAcyclicGraph<String> generateDetectedDependencyAndDeclaredDependencyGraph() {
    final DirectedAcyclicGraph<String> declaredDependencies = generateDeclaredDependencyGraph();
    final DirectedAcyclicGraph.Builder<String> graph = DirectedAcyclicGraph.serialBuilder();
    for (final String node : declaredDependencies.getNodes()) {
      graph.addNode(node);
      for (final String outgoingNode : declaredDependencies.getOutgoingNodesFor(node)) {
        graph.addEdge(node, outgoingNode);
      }
    }
    for (final UndeclaredDependency undeclaredDependency : undeclaredDependencies) {
      graph.addEdge(undeclaredDependency.sourceModule, undeclaredDependency.depModule);
    }
    return graph.build();
  }

  private DirectedAcyclicGraph<String> generateDeclaredDependencyGraph() {
    DirectedAcyclicGraph.Builder<String> declaredDependencyGraph =
        DirectedAcyclicGraph.serialBuilder();

    if (appModuleDependencies.isPresent()) {
      for (Map.Entry<String, ImmutableList<String>> moduleDependencies :
          appModuleDependencies.get().entrySet()) {
        for (String moduleDep : moduleDependencies.getValue()) {
          declaredDependencyGraph.addEdge(moduleDependencies.getKey(), moduleDep);
        }
      }
    }

    DirectedAcyclicGraph<String> result = declaredDependencyGraph.build();
    verifyNoUnrecognizedModulesInDependencyGraph(result);
    return result;
  }

  private void verifyNoUnrecognizedModulesInDependencyGraph(
      DirectedAcyclicGraph<String> dependencyGraph) {
    Set<String> configModules =
        getSeedConfigMap().isPresent() ? getSeedConfigMap().get().keySet() : new HashSet<>();
    Set<String> unrecognizedModules = new HashSet<>(dependencyGraph.getNodes());
    unrecognizedModules.removeAll(configModules);
    if (!unrecognizedModules.isEmpty()) {
      StringBuilder errorString =
          new StringBuilder("Unrecognized App Modules in Dependency Graph: ");
      for (String module : unrecognizedModules) {
        errorString.append(module).append(" ");
      }
      throw new IllegalStateException(errorString.toString());
    }
  }

  /**
   * This walks through the target graph starting from the root target and adds all reachable
   * targets that are not seed targets to the root module
   *
   * @return all reachable targets that are not seed targets to the root module
   */
  private ImmutableSet<BuildTargetType> generateRootTargets() {
    ImmutableSet.Builder<BuildTargetType> rootTargetsBuilder = new ImmutableSet.Builder<>();
    if (!targetGraph.isEmpty()) {
      Set<HasBuildTargetAndBuildDeps<BuildTargetType>> rootNodes = new HashSet<>();
      rootNodes.add(targetGraph.get(target));
      if (denylistModules.isPresent()) {
        for (BuildTargetType targetModule : denylistModules.get()) {
          rootNodes.add(targetGraph.get(targetModule));
          rootTargetsBuilder.add(targetModule);
        }
      }
      new AbstractBreadthFirstTraversal<HasBuildTargetAndBuildDeps<BuildTargetType>>(rootNodes) {
        @Override
        public Iterable<HasBuildTargetAndBuildDeps<BuildTargetType>> visit(
            HasBuildTargetAndBuildDeps<BuildTargetType> node) {

          ImmutableSet.Builder<HasBuildTargetAndBuildDeps<BuildTargetType>> depsBuilder =
              ImmutableSet.builder();
          for (BuildTargetType depTarget : node.getBuildDeps()) {
            if (!isSeedTarget(depTarget)) {
              depsBuilder.add(targetGraph.get(depTarget));
              rootTargetsBuilder.add(depTarget);
            }
          }
          return depsBuilder.build();
        }
      }.start();
    }
    return rootTargetsBuilder.build();
  }

  /**
   * For each seed target, find its reachable targets and mark them in a multimap as being reachable
   * by that module for later sorting into exclusive and shared targets
   *
   * @return the Multimap containing targets and the seed modules that contain them
   */
  private Multimap<BuildTargetType, String> mapTargetsToContainingModules() {
    final DirectedAcyclicGraph<String> declaredDependencies = getDeclaredDependencyGraph();
    final DirectedAcyclicGraph.Builder<String> moduleGraph = DirectedAcyclicGraph.serialBuilder();
    Multimap<BuildTargetType, String> targetToContainingApkModuleNameMap =
        MultimapBuilder.treeKeys().treeSetValues().build();
    for (Map.Entry<String, ImmutableList<BuildTargetType>> seedConfig :
        getSeedConfigMap().get().entrySet()) {
      final String seedModuleName = seedConfig.getKey();
      for (BuildTargetType seedTarget : seedConfig.getValue()) {
        final Set<BuildTargetType> seedBuildDeps = targetGraph.get(seedTarget).getBuildDeps();
        targetToContainingApkModuleNameMap.put(seedTarget, seedModuleName);
        new AbstractBreadthFirstTraversal<HasBuildTargetAndBuildDeps<BuildTargetType>>(
            targetGraph.get(seedTarget)) {
          @Override
          public ImmutableSet<HasBuildTargetAndBuildDeps<BuildTargetType>> visit(
              HasBuildTargetAndBuildDeps<BuildTargetType> node) {
            ImmutableSet.Builder<HasBuildTargetAndBuildDeps<BuildTargetType>> depsBuilder =
                ImmutableSet.builder();
            for (final BuildTargetType depTarget : node.getBuildDeps()) {
              final String depTargetModuleName = getSeedModule(depTarget);
              if (depTargetModuleName != null) {
                moduleGraph.addNode(depTargetModuleName);
              }
              if (!rootTargetsSupplier.get().contains(depTarget)) {
                if (isSeedTarget(depTarget)) {
                  if (depTargetModuleName != null
                      && depTargetModuleName != seedModuleName
                      && seedBuildDeps.contains(depTarget)) {
                    // Mark depTargetModuleName as a dependency of seedModuleName
                    moduleGraph.addEdge(seedModuleName, depTargetModuleName);
                  }
                } else {
                  depsBuilder.add(targetGraph.get(depTarget));
                  targetToContainingApkModuleNameMap.put(depTarget, seedModuleName);
                }
              }
            }
            return depsBuilder.build();
          }
        }.start();
      }
    }
    undeclaredDependencies.addAll(getUndeclaredDeps(moduleGraph.build(), declaredDependencies));

    // Now to generate the minimal covers of APKModules for each set of APKModules that contain
    // a buildTarget
    Multimap<BuildTargetType, String> targetModuleEntriesToRemove =
        MultimapBuilder.treeKeys().treeSetValues().build();
    for (BuildTargetType key : targetToContainingApkModuleNameMap.keySet()) {
      Collection<String> modulesForTarget = targetToContainingApkModuleNameMap.get(key);
      new AbstractBreadthFirstTraversal<String>(modulesForTarget) {
        @Override
        public Iterable<String> visit(String moduleName) throws RuntimeException {
          Collection<String> dependentModules =
              declaredDependencies.getIncomingNodesFor(moduleName);
          for (String dependent : dependentModules) {
            if (modulesForTarget.contains(dependent)) {
              targetModuleEntriesToRemove.put(key, dependent);
            }
          }
          return dependentModules;
        }
      }.start();
    }
    for (Map.Entry<BuildTargetType, String> entryToRemove : targetModuleEntriesToRemove.entries()) {
      targetToContainingApkModuleNameMap.remove(entryToRemove.getKey(), entryToRemove.getValue());
    }
    return targetToContainingApkModuleNameMap;
  }

  private Set<UndeclaredDependency> getUndeclaredDeps(
      final DirectedAcyclicGraph<String> detectedModuleGraph,
      final DirectedAcyclicGraph<String> declaredDeps) {
    final Set<UndeclaredDependency> undeclaredDependencies = new HashSet<>();
    final Set<String> addedModules = new HashSet<>();
    final Queue<String> moduleGraphQueue = new LinkedList<>();
    moduleGraphQueue.addAll(detectedModuleGraph.getNodesWithNoIncomingEdges());
    addedModules.addAll(detectedModuleGraph.getNodesWithNoIncomingEdges());
    while (!moduleGraphQueue.isEmpty()) {
      final String currentModule = moduleGraphQueue.poll();
      final Set<String> outgoingModuleGraphNodes =
          detectedModuleGraph.getOutgoingNodesFor(currentModule);
      final Set<String> outgoingDeclaredDepNodes = declaredDeps.getOutgoingNodesFor(currentModule);
      final Set<String> undeclaredDepNodes =
          Sets.difference(outgoingModuleGraphNodes, outgoingDeclaredDepNodes);
      if (!undeclaredDepNodes.isEmpty()) {
        for (final String undeclaredDepNode : undeclaredDepNodes) {
          if (undeclaredDepNode != null) {
            undeclaredDependencies.add(new UndeclaredDependency(currentModule, undeclaredDepNode));
          }
        }
      }
      for (final String edgeModule : outgoingModuleGraphNodes) {
        if (!addedModules.contains(edgeModule)) {
          moduleGraphQueue.add(edgeModule);
          addedModules.add(edgeModule);
        }
      }
    }
    return undeclaredDependencies;
  }

  /**
   * Loop through each of the targets we visited while generating seed modules: If the are exclusive
   * to that module, add them to that module. If they are not exclusive to that module, find or
   * create an appropriate shared module and fill out its dependencies
   *
   * @param apkModuleGraph the current graph we're building
   * @param targetToContainingApkModulesMap the targets mapped to the seed targets they are
   *     reachable from
   */
  private void generateSharedModules(
      MutableDirectedGraph<APKModule> apkModuleGraph,
      Multimap<BuildTargetType, String> targetToContainingApkModulesMap,
      Map<APKModule, ImmutableSet.Builder<BuildTargetType>> builder) {

    // Sort the module-covers of all targets to determine shared module names.
    TreeSet<TreeSet<String>> sortedContainingModuleSets =
        new TreeSet<>(
            new Comparator<TreeSet<String>>() {
              @Override
              public int compare(TreeSet<String> left, TreeSet<String> right) {
                int sizeDiff = left.size() - right.size();
                if (sizeDiff != 0) {
                  return sizeDiff;
                }
                Iterator<String> leftIter = left.iterator();
                Iterator<String> rightIter = right.iterator();
                while (leftIter.hasNext()) {
                  String leftElement = leftIter.next();
                  String rightElement = rightIter.next();
                  int stringComparison = leftElement.compareTo(rightElement);
                  if (stringComparison != 0) {
                    return stringComparison;
                  }
                }
                return 0;
              }
            });
    for (Map.Entry<BuildTargetType, Collection<String>> entry :
        targetToContainingApkModulesMap.asMap().entrySet()) {
      TreeSet<String> containingModuleSet = new TreeSet<>(entry.getValue());
      sortedContainingModuleSets.add(containingModuleSet);
    }

    // build modules based on all entries.
    Map<ImmutableSet<String>, APKModule> combinedModuleHashToModuleMap = new HashMap<>();
    final Set<String> hashedSharedModuleSet = new HashSet<>();
    for (TreeSet<String> moduleCover : sortedContainingModuleSets) {
      final List<String> moduleCoverList = new ArrayList<>(moduleCover);
      Collections.sort(moduleCoverList);
      final String joinedModuleName = String.join("_", moduleCoverList);
      String moduleName = moduleCoverList.size() > 1 ? "s_" + joinedModuleName : joinedModuleName;
      // there is a hard requirement that module names cannot exceed 50 chars.
      if (moduleCoverList.size() > 1 && moduleName.length() > 49) {
        moduleName = "s_" + String.valueOf(moduleName.hashCode() & 0x7FFFFFFF);
        if (hashedSharedModuleSet.contains(moduleName)) {
          throw new RuntimeException(
              "Collision while hashing shared module name " + joinedModuleName);
        }
        hashedSharedModuleSet.add(moduleName);
      }
      APKModule module = new APKModule(moduleName);
      combinedModuleHashToModuleMap.put(ImmutableSet.copyOf(moduleCover), module);
    }

    // add Targets per module;
    for (Map.Entry<BuildTargetType, Collection<String>> entry :
        targetToContainingApkModulesMap.asMap().entrySet()) {
      ImmutableSet<String> containingModuleSet = ImmutableSet.copyOf(entry.getValue());
      for (Map.Entry<ImmutableSet<String>, APKModule> existingEntry :
          combinedModuleHashToModuleMap.entrySet()) {
        if (existingEntry.getKey().equals(containingModuleSet)) {
          APKModule key = existingEntry.getValue();
          if (!builder.containsKey(key)) {
            builder.put(key, new ImmutableSet.Builder<>());
          }
          builder.get(key).add(entry.getKey());
          break;
        }
      }
    }

    // Find the seed modules and add them to the graph
    Map<String, APKModule> seedModules = new HashMap<>();
    for (Map.Entry<ImmutableSet<String>, APKModule> entry :
        combinedModuleHashToModuleMap.entrySet()) {
      if (entry.getKey().size() == 1) {
        APKModule seed = entry.getValue();
        apkModuleGraph.addNode(seed);
        seedModules.put(entry.getKey().iterator().next(), seed);
        apkModuleGraph.addEdge(seed, getRootAPKModule());
      }
    }

    // Find the shared modules and add them to the graph
    for (Map.Entry<ImmutableSet<String>, APKModule> entry :
        combinedModuleHashToModuleMap.entrySet()) {
      if (entry.getKey().size() > 1) {
        APKModule shared = entry.getValue();
        apkModuleGraph.addNode(shared);
        apkModuleGraph.addEdge(shared, getRootAPKModule());
        for (String seedName : entry.getKey()) {
          apkModuleGraph.addEdge(seedModules.get(seedName), shared);
        }
      }
    }
  }

  private boolean isSeedTarget(BuildTargetType depTarget) {
    if (!getSeedConfigMap().isPresent()) {
      return false;
    }
    return getSeedModule(depTarget) != null;
  }

  private String getSeedModule(final BuildTargetType seedTarget) {
    return getSeedTargetMap().get().get(seedTarget);
  }

  private void verifyNoSharedSeeds() {
    ImmutableMultimap<BuildTargetType, String> sharedSeeds = sharedSeedsSupplier.get();
    if (!sharedSeeds.isEmpty()) {
      StringBuilder errorMessage = new StringBuilder();
      for (BuildTargetType seed : sharedSeeds.keySet()) {
        errorMessage
            .append("BuildTargetType: ")
            .append(seed)
            .append(" is used as seed in multiple modules: ");
        for (String module : sharedSeeds.get(seed)) {
          errorMessage.append(module).append(' ');
        }
        errorMessage.append('\n');
      }
      throw new IllegalArgumentException(errorMessage.toString());
    }
  }

  private ImmutableMultimap<BuildTargetType, String> generateSharedSeeds() {
    Optional<ImmutableMap<String, ImmutableList<BuildTargetType>>> seedConfigMap =
        configMapSupplier.get();
    HashMultimap<BuildTargetType, String> sharedSeedMapBuilder = HashMultimap.create();
    if (!seedConfigMap.isPresent()) {
      return ImmutableMultimap.copyOf(sharedSeedMapBuilder);
    }
    // first: invert the seedConfigMap to get BuildTargetType -> Seeds
    for (Map.Entry<String, ImmutableList<BuildTargetType>> entry : seedConfigMap.get().entrySet()) {
      for (BuildTargetType buildTarget : entry.getValue()) {
        sharedSeedMapBuilder.put(buildTarget, entry.getKey());
      }
    }
    // second: remove keys that have only one value.
    Set<BuildTargetType> nonSharedSeeds = new HashSet<>();
    for (BuildTargetType buildTarget : sharedSeedMapBuilder.keySet()) {
      if (sharedSeedMapBuilder.get(buildTarget).size() <= 1) {
        nonSharedSeeds.add(buildTarget);
      }
    }
    for (BuildTargetType targetToRemove : nonSharedSeeds) {
      sharedSeedMapBuilder.removeAll(targetToRemove);
    }
    return ImmutableMultimap.copyOf(sharedSeedMapBuilder);
  }

  public ImmutableSet<BuildTargetType> getBuildTargets(APKModule module) {
    if (moduleToTarget.containsKey(module)) {
      return moduleToTarget.get(module);
    } else {
      return ImmutableSet.of();
    }
  }

  private class UndeclaredDependency {
    final String sourceModule;
    final String depModule;

    public UndeclaredDependency(final String sourceModule, final String depModule) {
      this.sourceModule = sourceModule;
      this.depModule = depModule;
    }

    @Override
    public String toString() {
      return "sourceModule:" + sourceModule + " depModule:" + depModule;
    }
  }
}
