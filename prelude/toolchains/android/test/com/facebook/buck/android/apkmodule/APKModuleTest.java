/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.apkmodule;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.in;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isIn;
import static org.hamcrest.Matchers.oneOf;

import com.facebook.buck.android.apkmodule.ExternalTargetGraph.ExternalBuildTarget;
import com.facebook.buck.android.apkmodule.ExternalTargetGraph.ExternalTargetNode;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSortedSet;
import com.google.common.collect.Iterables;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import org.junit.Test;

public class APKModuleTest {

  private void verifyDependencies(
      APKModuleGraph<ExternalBuildTarget> graph, APKModule module, ImmutableSet<String> names) {
    ImmutableSet<APKModule> deps = graph.getGraph().getOutgoingNodesFor(module);
    assertThat(deps.size(), is(names.size()));
    for (APKModule dep : deps) {
      assertThat(dep.getName(), in(names));
    }
  }

  private static ExternalTargetGraph buildGraph(Set<ExternalTargetNode> nodes) {
    ImmutableMap<ExternalBuildTarget, ExternalTargetNode> map =
        nodes.stream()
            .collect(
                ImmutableMap.toImmutableMap(
                    ExternalTargetNode::getBuildTarget, Function.identity()));
    ImmutableMap<String, ExternalBuildTarget> nameToBuildTargetMap =
        nodes.stream()
            .collect(
                ImmutableMap.toImmutableMap(
                    n -> n.getBuildTarget().getName(), ExternalTargetNode::getBuildTarget));
    return new ExternalTargetGraph(map, nameToBuildTargetMap);
  }

  /*
                         + - - - - - -+
                       ' root:      '
                       '            '
                       ' +--------+ '
       +-------------- ' | Binary | ' --------+
       |               ' +--------+ '         |
       |               '   |        '         |
       |               '   |        '         |
       v               '   |        '         v
   + - - - - - - +     '   |        '     +- - - - - +
   ' android:    '     '   |        '     ' java:    '
   '             '     '   |        '     '          '
   ' +---------+ '     '   |        '     ' +------+ '
   ' | Android | '     '   |        '     ' | Java | '
   ' +---------+ '     '   |        '     ' +------+ '
   '             '     '   |        '     '          '
   + - - - - - - +     '   |        '     +- - - - - +
       |               '   |        '         |
       |               '   |        '         |
       |               '   v        '         |
       |               ' +--------+ '         |
       +-------------> ' | Common | ' <-------+
                       ' +--------+ '
                       '            '
                       + - - - - - -+
  */
  @Test
  public void testAPKModuleGraphSimple() {
    ImmutableSet.Builder<ExternalTargetNode> nodeBuilder = ImmutableSet.builder();
    ExternalBuildTarget commonLibraryTarget =
        new ExternalBuildTarget("//src/com/facebook/test-common-library:test-common-library");
    nodeBuilder.add(new ExternalTargetNode(commonLibraryTarget));

    ExternalBuildTarget javaLibraryTarget =
        new ExternalBuildTarget("//src/com/facebook/test-java-library:test-java-library");
    nodeBuilder.add(new ExternalTargetNode(javaLibraryTarget, Set.of(commonLibraryTarget)));

    ExternalBuildTarget androidLibraryTarget =
        new ExternalBuildTarget("//src/com/facebook/test-android-library:test-android-library");
    nodeBuilder.add(new ExternalTargetNode(androidLibraryTarget, Set.of(commonLibraryTarget)));

    ExternalBuildTarget keystoreTarget = new ExternalBuildTarget("//:keystore");
    nodeBuilder.add(new ExternalTargetNode(keystoreTarget));

    ExternalBuildTarget androidBinaryTarget =
        new ExternalBuildTarget("//src/com/facebook/test-android-binary:test-android-binary");
    nodeBuilder.add(
        new ExternalTargetNode(
            androidBinaryTarget,
            ImmutableSortedSet.of(androidLibraryTarget, javaLibraryTarget, commonLibraryTarget)));

    ExternalTargetGraph graph = buildGraph(nodeBuilder.build());

    ImmutableMap.Builder<String, ImmutableList<ExternalBuildTarget>> seedConfigMap =
        ImmutableMap.builder();
    seedConfigMap.put("android", ImmutableList.of(androidLibraryTarget));
    seedConfigMap.put("java", ImmutableList.of(javaLibraryTarget));

    APKModuleGraph<ExternalBuildTarget> dag =
        new APKModuleGraph<>(
            graph,
            androidBinaryTarget,
            Optional.of(seedConfigMap.build()),
            Optional.empty(),
            Optional.empty());

    ImmutableSet<APKModule> topLevelNodes = dag.getGraph().getNodesWithNoIncomingEdges();
    assertThat(topLevelNodes.size(), is(2));

    for (APKModule apkModule : topLevelNodes) {
      assertThat(apkModule.getName(), oneOf("android", "java"));
      ImmutableSet<APKModule> dependencies = dag.getGraph().getOutgoingNodesFor(apkModule);
      assertThat(apkModule.isRootModule(), is(false));

      assertThat(dependencies.size(), is(1));
      assertThat(
          Iterables.getFirst(dependencies, null).getName(), is(APKModule.ROOT_APKMODULE_NAME));
      assertThat(Iterables.getFirst(dependencies, null).isRootModule(), is(true));
    }

    assertThat(
        dag.getAPKModules().stream().map(APKModule::getName).collect(ImmutableSet.toImmutableSet()),
        containsInAnyOrder("dex", "android", "java"));
  }

  /*
                           +- - - - - - - - - - - - +
                           ' root:                  '
                           '                        '
                           ' +--------------------+ '
      +------------------- ' |       Binary       | ' -------------+
      |                    ' +--------------------+ '              |
      |                    '   |                    '              |
      |                    '   |                    '              |
      v                    '   |                    '              v
  + - - - - - - +          '   |                    '          +- - - - - +
  ' android:    '          '   |                    '          ' java:    '
  '             '          '   v                    '          '          '
  ' +---------+ '          ' +--------------------+ '          ' +------+ '
  ' | Android | ' =======> ' |       Common       | ' <======= ' | Java | '
  ' +---------+ '          ' +--------------------+ '          ' +------+ '
  '             '          '                        '          '          '
  + - - - - - - +          +- - - - - - - - - - - - +          +- - - - - +
      H                                                            H
      H                                                            H
      H                                                            H
      H                    +- - - - - - - - - - - - +              H
      H                    ' shared_android_java:   '              H
      H                    '                        '              H
      H                    ' +--------------------+ '              H
      #==================> ' |       Shared       | ' <============#
                           ' +--------------------+ '
                           '                        '
                           +- - - - - - - - - - - - +

     -- target dependecy
     == package and target dependecy
     */
  @Test
  public void testAPKModuleGraphSharedDependency() {

    ImmutableSet.Builder<ExternalTargetNode> nodeBuilder = ImmutableSet.builder();
    ExternalBuildTarget commonLibraryTarget = new ExternalBuildTarget("//:test-common-library");
    nodeBuilder.add(new ExternalTargetNode(commonLibraryTarget));

    ExternalBuildTarget sharedLibraryTarget = new ExternalBuildTarget("//:test-shared-library");
    nodeBuilder.add(new ExternalTargetNode(sharedLibraryTarget, Set.of(commonLibraryTarget)));

    ExternalBuildTarget javaLibraryTarget = new ExternalBuildTarget("//:test-java-library");
    nodeBuilder.add(
        new ExternalTargetNode(
            javaLibraryTarget, Set.of(commonLibraryTarget, sharedLibraryTarget)));

    ExternalBuildTarget androidLibraryTarget = new ExternalBuildTarget("//:test-android-library");
    nodeBuilder.add(
        new ExternalTargetNode(
            androidLibraryTarget, Set.of(commonLibraryTarget, sharedLibraryTarget)));

    ExternalBuildTarget keystoreTarget = new ExternalBuildTarget("//:keystore");
    nodeBuilder.add(new ExternalTargetNode(keystoreTarget));

    ExternalBuildTarget androidBinaryTarget = new ExternalBuildTarget("//:test-android-binary");
    nodeBuilder.add(
        new ExternalTargetNode(
            androidBinaryTarget,
            ImmutableSortedSet.of(androidLibraryTarget, javaLibraryTarget, commonLibraryTarget)));

    ExternalTargetGraph graph = buildGraph(nodeBuilder.build());

    ImmutableMap.Builder<String, ImmutableList<ExternalBuildTarget>> seedConfigMap =
        ImmutableMap.builder();
    seedConfigMap.put("test.android.library", ImmutableList.of(androidLibraryTarget));
    seedConfigMap.put("test.java.library", ImmutableList.of(javaLibraryTarget));

    APKModuleGraph<ExternalBuildTarget> dag =
        new APKModuleGraph<>(
            graph,
            androidBinaryTarget,
            Optional.of(seedConfigMap.build()),
            Optional.empty(),
            Optional.empty());

    ImmutableSet<APKModule> topLevelNodes = dag.getGraph().getNodesWithNoIncomingEdges();
    assertThat(topLevelNodes.size(), is(2));

    final List<String> expectedModuleNames = new ArrayList<>();
    expectedModuleNames.add("test.android.library");
    expectedModuleNames.add("test.java.library");
    Collections.sort(expectedModuleNames);
    final String sharedModuleName = "s_" + String.join("_", expectedModuleNames);
    for (APKModule apkModule : topLevelNodes) {
      assertThat(apkModule.getName(), isIn(expectedModuleNames));
      ImmutableSet<APKModule> dependencies = dag.getGraph().getOutgoingNodesFor(apkModule);

      assertThat(dependencies.size(), is(2));
      assertThat(
          Iterables.getFirst(dependencies, null).getName(), is(APKModule.ROOT_APKMODULE_NAME));
      assertThat(Iterables.getLast(dependencies, null).getName(), is(sharedModuleName));
    }
  }

  /*
                             +- - - - - - - - - - - - +
                             ' root:                  '
                             '                        '
                             ' +--------------------+ '
        +------------------- ' |       Binary       | ' ---------------------+
        |                    ' +--------------------+ '                      |
        |                    '   |                    '                      |
        |                    '   |                    '                      |
        v                    '   |                    '                      v
    + - - - - - - +          '   |                    '          +- - - - - - - - - - - - +
    ' android:    '          '   |                    '          ' java:                  '
    '             '          '   v                    '          '                        '
    ' +---------+ '          ' +--------------------+ '          ' +--------------------+ '
    ' | Android | ' =======> ' |       Common       | ' <======= ' |        Java        | '
    ' +---------+ '          ' +--------------------+ '          ' +--------------------+ '
    '             '          '                        '          '           |            '
    + - - - - - - +          +- - - - - - - - - - - - +          '           |            '
        H                                                        '           v            '
        H                                                        ' +--------------------+ '
        #======================================================> ' |       Shared       | '
                                                                 ' +--------------------+ '
                                                                 '                        '
                                                                 +- - - - - - - - - - - - +

       Android module depends on the Java Module
       -- target dependecy
       == package and target dependecy
  */
  @Test
  public void testAPKModuleGraphWithDeclaredDependency() {
    ImmutableSet.Builder<ExternalTargetNode> nodeBuilder = ImmutableSet.builder();
    ExternalBuildTarget commonLibraryTarget = new ExternalBuildTarget("//:test-common-library");
    nodeBuilder.add(new ExternalTargetNode(commonLibraryTarget));

    ExternalBuildTarget sharedLibraryTarget = new ExternalBuildTarget("//:test-shared-library");
    nodeBuilder.add(new ExternalTargetNode(sharedLibraryTarget, Set.of(commonLibraryTarget)));

    ExternalBuildTarget javaLibraryTarget = new ExternalBuildTarget("//:test-java-library");
    nodeBuilder.add(
        new ExternalTargetNode(
            javaLibraryTarget, Set.of(commonLibraryTarget, sharedLibraryTarget)));

    ExternalBuildTarget androidLibraryTarget = new ExternalBuildTarget("//:test-android-library");
    nodeBuilder.add(
        new ExternalTargetNode(
            androidLibraryTarget, Set.of(commonLibraryTarget, sharedLibraryTarget)));

    ExternalBuildTarget keystoreTarget = new ExternalBuildTarget("//:keystore");
    nodeBuilder.add(new ExternalTargetNode(keystoreTarget));

    ExternalBuildTarget androidBinaryTarget = new ExternalBuildTarget("//:test-android-binary");
    nodeBuilder.add(
        new ExternalTargetNode(
            androidBinaryTarget,
            ImmutableSortedSet.of(androidLibraryTarget, javaLibraryTarget, commonLibraryTarget)));

    ExternalTargetGraph graph = buildGraph(nodeBuilder.build());

    ImmutableMap.Builder<String, ImmutableList<ExternalBuildTarget>> seedConfigMap =
        ImmutableMap.builder();

    seedConfigMap.put("android", ImmutableList.of(androidLibraryTarget));

    seedConfigMap.put("java", ImmutableList.of(javaLibraryTarget));

    ImmutableMap.Builder<String, ImmutableList<String>> appModuleDependencies =
        ImmutableMap.builder();
    appModuleDependencies.put("android", ImmutableList.of("java"));

    APKModuleGraph<ExternalBuildTarget> dag =
        new APKModuleGraph<>(
            graph,
            androidBinaryTarget,
            Optional.of(seedConfigMap.build()),
            Optional.of(appModuleDependencies.build()),
            Optional.empty());

    ImmutableSet<APKModule> topLevelNodes = dag.getGraph().getNodesWithNoIncomingEdges();
    assertThat(topLevelNodes.size(), is(1));

    APKModule topModule = Iterables.getFirst(topLevelNodes, null);
    assertThat(topModule.getName(), is("android"));

    ImmutableSet<APKModule> dependencies = dag.getGraph().getOutgoingNodesFor(topModule);
    assertThat(dependencies.size(), is(2));

    for (APKModule dependency : dependencies) {
      assertThat(dependency.getName(), oneOf(APKModule.ROOT_APKMODULE_NAME, "java"));
      if (dependency.getName().equals("java")) {
        ImmutableSet<APKModule> javaDeps = dag.getGraph().getOutgoingNodesFor(dependency);
        assertThat(javaDeps.size(), is(1));
        assertThat(Iterables.getFirst(javaDeps, null).getName(), is(APKModule.ROOT_APKMODULE_NAME));
      }
    }
  }

  /*
                        +- - - - - - - - - - - - +
                        ' root:                  '
                        '                        '
                        ' +--------------------+ ' -----------------------+
        +-------------- ' |       Binary       | ' --------+              |
        |               ' +--------------------+ '         |              |
        |               '   |                    '         |              |
        |               '   |                    '         |              |
        v               '   |                    '         v              v
    + - - - - - - +     '   |                    '    + - - - - - +   + - - - - - +
    ' android:    '     '   |                    '    ' java:     '   ' java2:    '
    '             '     '   v                    '    '           '   '           '
    ' +---------+ '     ' +--------------------+ '    ' +-------+ '   ' +-------+ '
    ' | Android | ' ==> ' |       Common       | ' <= ' | Java  | '   ' | Java2 | '
    ' +---------+ '     ' +--------------------+ '    ' +-------+ '   ' +-------+ '
    '             '     '                        '    +- - - - - -+   + - - - - - +
    + - - - - - - +     +- - - - - - - - - - - - +           H            H  H
        |                            ^                       H            H  H
        |                            H                       H            H  H
        |                            #=======================#============#  H
        |                                                    H               H
        |                                                    v               H
        |                                             + - - - - - +          H
        |                                             ' shared:   '          H
        |                                             '           '          H
        |                                             ' +-------+ '          H
        +-------------------------------------------> ' |Shared | ' <========#
                                                      ' +-------+ '
                                                      + - - - - - +

       Android module depends on the Java module.
       -- target dependecy
       == package and target dependecy
  */

  @Test
  public void testAPKModuleGraphSharedWithDeclaredDependency() {

    ImmutableSet.Builder<ExternalTargetNode> nodeBuilder = ImmutableSet.builder();
    ExternalBuildTarget commonLibraryTarget = new ExternalBuildTarget("//:test-common-library");
    nodeBuilder.add(new ExternalTargetNode(commonLibraryTarget));

    ExternalBuildTarget sharedLibraryTarget = new ExternalBuildTarget("//:test-shared-library");
    nodeBuilder.add(new ExternalTargetNode(sharedLibraryTarget, Set.of(commonLibraryTarget)));

    ExternalBuildTarget javaLibraryTarget = new ExternalBuildTarget("//:test-java-library");
    nodeBuilder.add(
        new ExternalTargetNode(
            javaLibraryTarget, Set.of(commonLibraryTarget, sharedLibraryTarget)));

    ExternalBuildTarget java2LibraryTarget = new ExternalBuildTarget("//:test-java2-library");
    nodeBuilder.add(
        new ExternalTargetNode(
            java2LibraryTarget, Set.of(commonLibraryTarget, sharedLibraryTarget)));

    ExternalBuildTarget androidLibraryTarget = new ExternalBuildTarget("//:test-android-library");
    nodeBuilder.add(
        new ExternalTargetNode(
            androidLibraryTarget, Set.of(commonLibraryTarget, sharedLibraryTarget)));

    ExternalBuildTarget keystoreTarget = new ExternalBuildTarget("//:keystore");
    nodeBuilder.add(new ExternalTargetNode(keystoreTarget));

    ExternalBuildTarget androidBinaryTarget = new ExternalBuildTarget("//:test-android-binary");
    nodeBuilder.add(
        new ExternalTargetNode(
            androidBinaryTarget,
            ImmutableSortedSet.of(
                androidLibraryTarget, javaLibraryTarget, java2LibraryTarget, commonLibraryTarget)));

    ExternalTargetGraph graph = buildGraph(nodeBuilder.build());

    ImmutableMap.Builder<String, ImmutableList<ExternalBuildTarget>> seedConfigMap =
        ImmutableMap.builder();

    seedConfigMap.put("android", ImmutableList.of(androidLibraryTarget));

    seedConfigMap.put("java", ImmutableList.of(javaLibraryTarget));

    seedConfigMap.put("java2", ImmutableList.of(java2LibraryTarget));

    ImmutableMap.Builder<String, ImmutableList<String>> appModuleDependencies =
        ImmutableMap.builder();
    appModuleDependencies.put("android", ImmutableList.of("java"));

    APKModuleGraph<ExternalBuildTarget> dag =
        new APKModuleGraph<>(
            graph,
            androidBinaryTarget,
            Optional.of(seedConfigMap.build()),
            Optional.of(appModuleDependencies.build()),
            Optional.empty());

    ImmutableSet<APKModule> topLevelNodes = dag.getGraph().getNodesWithNoIncomingEdges();
    assertThat(topLevelNodes.size(), is(2));

    for (APKModule topModule : topLevelNodes) {
      assertThat(topModule.getName(), oneOf("java2", "android"));
      switch (topModule.getName()) {
        case "java2":
          Set<APKModule> java2Dependencies = dag.getGraph().getOutgoingNodesFor(topModule);
          for (APKModule dependency : java2Dependencies) {
            assertThat(dependency.getName(), oneOf(APKModule.ROOT_APKMODULE_NAME, "s_java_java2"));
          }
          break;
        case "android":
          Set<APKModule> androidDependencies = dag.getGraph().getOutgoingNodesFor(topModule);
          for (APKModule dependency : androidDependencies) {
            assertThat(dependency.getName(), oneOf(APKModule.ROOT_APKMODULE_NAME, "java"));
            if (dependency.getName().equals("java")) {
              Set<APKModule> javaDeps = dag.getGraph().getOutgoingNodesFor(dependency);
              for (APKModule javaDep : javaDeps) {
                assertThat(javaDep.getName(), oneOf(APKModule.ROOT_APKMODULE_NAME, "s_java_java2"));
              }
            }
          }
          break;
        default:
      }
    }
  }

  /*
                             +- - - - - - - - - - - - +
                             ' root:                  '
                             '                        '
                             ' +--------------------+ '
        +------------------- ' |       Binary       | ' ---------------------+
        |                    ' +--------------------+ '                      |
        |                                                                    |
        |                                                                    |
        v                                                                    v
    + - - - - - - +                                              +- - - - - - - - - - - - +
    ' android:    '                                              ' java:                  '
    '             '                                              '                        '
    ' +---------+ '                                              ' +--------------------+ '
    ' | Android | ' -------------------------------------------> ' |        Java        | '
    ' +---------+ '                                              ' +--------------------+ '
    '             '                                              '                        '
    + - - - - - - +                                              +- - - - - - - - - - - - +


       Android module depends on the Java Module
       -- target dependecy
       == package and target dependecy
  */
  @Test
  public void testAPKModuleGraphWithMissingDeclaredDependency() {
    ImmutableSet.Builder<ExternalTargetNode> nodeBuilder = ImmutableSet.builder();
    ExternalBuildTarget javaLibraryTarget = new ExternalBuildTarget("//:test-java-library");
    nodeBuilder.add(new ExternalTargetNode(javaLibraryTarget));

    ExternalBuildTarget androidLibraryTarget = new ExternalBuildTarget("//:test-android-library");
    nodeBuilder.add(new ExternalTargetNode(androidLibraryTarget, Set.of(javaLibraryTarget)));

    ExternalBuildTarget keystoreTarget = new ExternalBuildTarget("//:keystore");
    nodeBuilder.add(new ExternalTargetNode(keystoreTarget));

    ExternalBuildTarget androidBinaryTarget = new ExternalBuildTarget("//:test-android-binary");
    nodeBuilder.add(
        new ExternalTargetNode(
            androidBinaryTarget, ImmutableSortedSet.of(androidLibraryTarget, javaLibraryTarget)));

    ExternalTargetGraph graph = buildGraph(nodeBuilder.build());

    ImmutableMap.Builder<String, ImmutableList<ExternalBuildTarget>> seedConfigMap =
        ImmutableMap.builder();
    seedConfigMap.put("android", ImmutableList.of(androidLibraryTarget));
    seedConfigMap.put("java", ImmutableList.of(javaLibraryTarget));

    APKModuleGraph<ExternalBuildTarget> dag =
        new APKModuleGraph<>(
            graph,
            androidBinaryTarget,
            Optional.of(seedConfigMap.build()),
            Optional.empty(),
            Optional.empty());

    ImmutableSet<APKModule> topLevelNodes = dag.getGraph().getNodesWithNoIncomingEdges();
    assertThat(topLevelNodes.size(), is(1));

    APKModule topModule = Iterables.getFirst(topLevelNodes, null);
    assertThat(topModule.getName(), is("android"));

    ImmutableSet<APKModule> dependencies = dag.getGraph().getOutgoingNodesFor(topModule);
    assertThat(dependencies.size(), is(2));

    for (APKModule dependency : dependencies) {
      assertThat(dependency.getName(), oneOf(APKModule.ROOT_APKMODULE_NAME, "java"));
      if (dependency.getName().equals("java")) {
        ImmutableSet<APKModule> javaDeps = dag.getGraph().getOutgoingNodesFor(dependency);
        assertThat(javaDeps.size(), is(1));
        assertThat(Iterables.getFirst(javaDeps, null).getName(), is(APKModule.ROOT_APKMODULE_NAME));
      }
    }
  }

  /*
                             +- - - - - - - - - - - - +
                             ' root:                  '
                             '                        '
                             ' +--------------------+ '
        +------------------- ' |       Binary       | ' ---------------------+
        |                    ' +--------------------+ '                      |
        |                                                                    |
        |                                                                    |
        v                                                                    v
    + - - - - - - +                                              +- - - - - - - - - - - - +
    ' android:    '                                              ' java:                  '
    '             '                                              '                        '
    ' +---------+ '                                              ' +--------------------+ '
    ' | Android | ' ===========================================> ' |        Java        | '
    ' +---------+ '                                              ' +--------------------+ '
    '             '                                              '          |             '
    + - - - - - - +                                              '          |             '
                                                                 '          |             '
                                                                 '          v             '
                                                                 ' +--------------------+ '
                                                                 ' |    Java Dep        | '
                                                                 ' +--------------------+ '
                                                                 +- - - - - - - - - - - - +


       Android module depends on the Java Module
       -- target dependecy
       == package and target dependecy
  */
  @Test
  public void testAPKModuleGraphWithUndeclaredTransitiveDeps() {
    ImmutableSet.Builder<ExternalTargetNode> nodeBuilder = ImmutableSet.builder();
    ExternalBuildTarget javaDepLibraryTarget = new ExternalBuildTarget("//:test-java-dep-library");
    nodeBuilder.add(new ExternalTargetNode(javaDepLibraryTarget));

    ExternalBuildTarget javaLibraryTarget = new ExternalBuildTarget("//:test-java-library");
    nodeBuilder.add(new ExternalTargetNode(javaLibraryTarget, Set.of(javaDepLibraryTarget)));

    ExternalBuildTarget androidLibraryTarget = new ExternalBuildTarget("//:test-android-library");
    nodeBuilder.add(new ExternalTargetNode(androidLibraryTarget, Set.of(javaLibraryTarget)));

    ExternalBuildTarget keystoreTarget = new ExternalBuildTarget("//:keystore");
    nodeBuilder.add(new ExternalTargetNode(keystoreTarget));

    ExternalBuildTarget androidBinaryTarget = new ExternalBuildTarget("//:test-android-binary");
    nodeBuilder.add(
        new ExternalTargetNode(
            androidBinaryTarget, ImmutableSortedSet.of(androidLibraryTarget, javaLibraryTarget)));

    ExternalTargetGraph graph = buildGraph(nodeBuilder.build());

    ImmutableMap.Builder<String, ImmutableList<ExternalBuildTarget>> seedConfigMap =
        ImmutableMap.builder();
    seedConfigMap.put("android", ImmutableList.of(androidLibraryTarget));
    seedConfigMap.put("java", ImmutableList.of(javaLibraryTarget));
    seedConfigMap.put("java-dep", ImmutableList.of(javaDepLibraryTarget));

    APKModuleGraph<ExternalBuildTarget> dag =
        new APKModuleGraph<>(
            graph,
            androidBinaryTarget,
            Optional.of(seedConfigMap.build()),
            Optional.empty(),
            Optional.empty());

    ImmutableSet<APKModule> topLevelNodes = dag.getGraph().getNodesWithNoIncomingEdges();
    assertThat(topLevelNodes.size(), is(1));

    APKModule topModule = Iterables.getFirst(topLevelNodes, null);
    assertThat(topModule.getName(), is("android"));

    ImmutableSet<APKModule> dependencies = dag.getGraph().getOutgoingNodesFor(topModule);
    assertThat(dependencies.size(), is(2));

    for (final APKModule dependency : dependencies) {
      assertThat(dependency.getName(), oneOf(APKModule.ROOT_APKMODULE_NAME, "java", "java-dep"));
      if (dependency.getName().equals("java")) {
        ImmutableSet<APKModule> javaDeps = dag.getGraph().getOutgoingNodesFor(dependency);
        assertThat(javaDeps.size(), is(2));
        for (final APKModule javaDep : javaDeps) {
          assertThat(javaDep.getName(), oneOf(APKModule.ROOT_APKMODULE_NAME, "java-dep"));
        }
      } else if (dependency.getName().equals("java-dep")) {
        ImmutableSet<APKModule> javaDeps = dag.getGraph().getOutgoingNodesFor(dependency);
        assertThat(javaDeps.size(), is(1));
        assertThat(Iterables.getFirst(javaDeps, null).getName(), is(APKModule.ROOT_APKMODULE_NAME));
      }
    }
  }

  /*

                                                             + - - - - - - - - - +
                                                             ' root:             '
                                                             '                   '
              +--------------------------------------------- ' +---------------+ '
              |             +------------------------------- ' |    Binary     | '
              v             |              +---------------- ' +---------------+ '
     + - - - - - - +        |              |                 '         |         '
     ' android:    '        |              |                 '         |         '
     '             '        |              |                 '         v         '
     ' +---------+ ' =======+==============+===#             ' +---------------+ '
     ' | Android | '        v              |   H             ' |               | '
     ' +---------+ '   + - - - - - +       |   #===========> ' |               | '
     '             '   ' java:     '       |                 ' |               | '
     + - - - - - - +   '           '       |                 ' |               | '
           |   H       ' +-------+ ' ======+===============> ' |               | '
           |   #=====> ' | Java  | '       v                 ' |    Common     | '
           |           ' +-------+ '   + - - - - - - - +     ' |               | '
           |           '           '   ' java2:        '     ' |               | '
           |           + - - - - - +   '               '     ' |               | '
           |               |   H       ' +-----------+ '     ' |               | '
           |               |   #=====> ' |   Java2   | ' ==> ' |               | '
           |               |           ' +-----------+ '     ' +---------------+ '
           |               |           '               '     '                   '
           |               |           '               '     + - - - - - - - - - +
           |               +---------> ' +-----------+ '
           |                           ' |  Shared   | '
           +-------------------------> ' +-----------+ '
                                       '               '
                                       + - - - - - - - +

     There is a declared dependency from android -> java and java -> java2.
  */

  @Test
  public void testAPKModuleGraphWithMultiLevelDependencies() {

    ImmutableSet.Builder<ExternalTargetNode> nodeBuilder = ImmutableSet.builder();
    ExternalBuildTarget commonLibraryTarget = new ExternalBuildTarget("//:test-common-library");
    nodeBuilder.add(new ExternalTargetNode(commonLibraryTarget));

    ExternalBuildTarget sharedLibraryTarget = new ExternalBuildTarget("//:test-shared-library");
    nodeBuilder.add(new ExternalTargetNode(sharedLibraryTarget, Set.of(commonLibraryTarget)));

    ExternalBuildTarget java2LibraryTarget = new ExternalBuildTarget("//:test-java2-library");
    nodeBuilder.add(
        new ExternalTargetNode(
            java2LibraryTarget, Set.of(commonLibraryTarget, sharedLibraryTarget)));

    ExternalBuildTarget javaLibraryTarget = new ExternalBuildTarget("//:test-java-library");
    nodeBuilder.add(
        new ExternalTargetNode(
            javaLibraryTarget,
            Set.of(commonLibraryTarget, sharedLibraryTarget, java2LibraryTarget)));

    ExternalBuildTarget androidLibraryTarget = new ExternalBuildTarget("//:test-android-library");
    nodeBuilder.add(
        new ExternalTargetNode(
            androidLibraryTarget,
            Set.of(
                commonLibraryTarget, sharedLibraryTarget, java2LibraryTarget, javaLibraryTarget)));

    ExternalBuildTarget keystoreTarget = new ExternalBuildTarget("//:keystore");
    nodeBuilder.add(new ExternalTargetNode(keystoreTarget));

    ExternalBuildTarget androidBinaryTarget = new ExternalBuildTarget("//:test-android-binary");
    nodeBuilder.add(
        new ExternalTargetNode(
            androidBinaryTarget,
            ImmutableSortedSet.of(
                androidLibraryTarget, javaLibraryTarget, java2LibraryTarget, commonLibraryTarget)));

    ExternalTargetGraph graph = buildGraph(nodeBuilder.build());

    ImmutableMap.Builder<String, ImmutableList<ExternalBuildTarget>> seedConfigMap =
        ImmutableMap.builder();

    seedConfigMap.put("android", ImmutableList.of(androidLibraryTarget));
    seedConfigMap.put("java", ImmutableList.of(javaLibraryTarget));
    seedConfigMap.put("java2", ImmutableList.of(java2LibraryTarget));

    ImmutableMap.Builder<String, ImmutableList<String>> appModuleDependencies =
        ImmutableMap.builder();

    appModuleDependencies.put("android", ImmutableList.of("java", "java2"));
    appModuleDependencies.put("java", ImmutableList.of("java2"));

    APKModuleGraph<ExternalBuildTarget> dag =
        new APKModuleGraph<>(
            graph,
            androidBinaryTarget,
            Optional.of(seedConfigMap.build()),
            Optional.of(appModuleDependencies.build()),
            Optional.empty());

    ImmutableSet<APKModule> topLevelNodes = dag.getGraph().getNodesWithNoIncomingEdges();
    assertThat(topLevelNodes.size(), is(1));

    APKModule topModule = Iterables.getFirst(topLevelNodes, null);
    assertThat(topModule.getName(), is("android"));

    ImmutableSet<APKModule> topLevelDeps = dag.getGraph().getOutgoingNodesFor(topModule);
    assertThat(topLevelDeps.size(), is(3));

    APKModule middleModule = null;
    for (APKModule apkModule : topLevelDeps) {
      assertThat(apkModule.getName(), oneOf(APKModule.ROOT_APKMODULE_NAME, "java", "java2"));
      if (apkModule.getName().equals("java")) {
        middleModule = apkModule;
      }
    }

    ImmutableSet<APKModule> middleLevelDeps = dag.getGraph().getOutgoingNodesFor(middleModule);
    assertThat(middleLevelDeps.size(), is(2));

    APKModule bottomModule = null;
    for (APKModule apkModule : middleLevelDeps) {
      assertThat(apkModule.getName(), oneOf(APKModule.ROOT_APKMODULE_NAME, "java2"));
      if (apkModule.getName().equals("java2")) {
        bottomModule = apkModule;
      }
    }

    ImmutableSet<APKModule> bottomLevelDeps = dag.getGraph().getOutgoingNodesFor(bottomModule);
    assertThat(bottomLevelDeps.size(), is(1));

    APKModule bottomDep = Iterables.getFirst(bottomLevelDeps, null);
    assertThat(bottomDep.getName(), is(APKModule.ROOT_APKMODULE_NAME));
  }

  /*

                                                             + - - - - - - - - - +
                                                             ' root:             '
                                                             '                   '
              +--------------------------------------------- ' +---------------+ '
              |             +------------------------------- ' |    Binary     | '
              v             |              +---------------- ' +---------------+ '
     + - - - - - - +        |              |                 '         |         '
     ' android:    '        |              |                 '         |         '
     '             '        |              |                 '         v         '
     ' +---------+ ' =======+==============+===#             ' +---------------+ '
     ' | Android | '        v              |   H             ' |               | '
     ' +---------+ '   + - - - - - +       |   #===========> ' |               | '
     '             '   ' java:     '       |                 ' |               | '
     + - - - - - - +   '           '       |                 ' |               | '
           |   H       ' +-------+ ' ======+===============> ' |               | '
           |   #=====> ' | Java  | '       v                 ' |    Common     | '
           |           ' +-------+ '   + - - - - - - - +     ' |               | '
           |           '           '   ' java2:        '     ' |               | '
           |           + - - - - - +   '               '     ' |               | '
           |                           ' +-----------+ '     ' |               | '
           |                           ' |   Java2   | ' ==> ' |               | '
           |                           ' +-----------+ '     ' +---------------+ '
           |                           '               '     '                   '
           |                           '               '     + - - - - - - - - - +
           |                           ' +-----------+ '
           |                           ' |  Shared   | '
           +-------------------------> ' +-----------+ '
                                       '               '
                                       + - - - - - - - +

     There is a declared dependency from android -> java and java -> java2.
  */

  @Test
  public void testAPKModuleGraphThatLowestDeclaredDepTakesCareOfMultipleLevelsOfIndirection() {

    ImmutableSet.Builder<ExternalTargetNode> nodeBuilder = ImmutableSet.builder();
    ExternalBuildTarget commonLibraryTarget = new ExternalBuildTarget("//:test-common-library");
    nodeBuilder.add(new ExternalTargetNode(commonLibraryTarget));

    ExternalBuildTarget sharedLibraryTarget = new ExternalBuildTarget("//:test-shared-library");
    nodeBuilder.add(new ExternalTargetNode(sharedLibraryTarget, Set.of(commonLibraryTarget)));

    ExternalBuildTarget java2LibraryTarget = new ExternalBuildTarget("//:test-java2-library");
    nodeBuilder.add(
        new ExternalTargetNode(
            java2LibraryTarget, Set.of(commonLibraryTarget, sharedLibraryTarget)));

    ExternalBuildTarget javaLibraryTarget = new ExternalBuildTarget("//:test-java-library");
    nodeBuilder.add(
        new ExternalTargetNode(javaLibraryTarget, Set.of(commonLibraryTarget, java2LibraryTarget)));

    ExternalBuildTarget androidLibraryTarget = new ExternalBuildTarget("//:test-android-library");
    nodeBuilder.add(
        new ExternalTargetNode(
            androidLibraryTarget,
            Set.of(
                commonLibraryTarget, sharedLibraryTarget, java2LibraryTarget, javaLibraryTarget)));

    ExternalBuildTarget keystoreTarget = new ExternalBuildTarget("//:keystore");
    nodeBuilder.add(new ExternalTargetNode(keystoreTarget));

    ExternalBuildTarget androidBinaryTarget = new ExternalBuildTarget("//:test-android-binary");
    nodeBuilder.add(
        new ExternalTargetNode(
            androidBinaryTarget,
            ImmutableSortedSet.of(
                androidLibraryTarget, javaLibraryTarget, java2LibraryTarget, commonLibraryTarget)));

    ExternalTargetGraph graph = buildGraph(nodeBuilder.build());

    ImmutableMap.Builder<String, ImmutableList<ExternalBuildTarget>> seedConfigMap =
        ImmutableMap.builder();

    seedConfigMap.put("android", ImmutableList.of(androidLibraryTarget));
    seedConfigMap.put("java", ImmutableList.of(javaLibraryTarget));
    seedConfigMap.put("java2", ImmutableList.of(java2LibraryTarget));

    ImmutableMap.Builder<String, ImmutableList<String>> appModuleDependencies =
        ImmutableMap.builder();

    appModuleDependencies.put("android", ImmutableList.of("java", "java2"));
    appModuleDependencies.put("java", ImmutableList.of("java2"));

    APKModuleGraph<ExternalBuildTarget> dag =
        new APKModuleGraph<>(
            graph,
            androidBinaryTarget,
            Optional.of(seedConfigMap.build()),
            Optional.of(appModuleDependencies.build()),
            Optional.empty());

    ImmutableSet<APKModule> topLevelNodes = dag.getGraph().getNodesWithNoIncomingEdges();
    assertThat(topLevelNodes.size(), is(1));

    APKModule topModule = Iterables.getFirst(topLevelNodes, null);
    assertThat(topModule.getName(), is("android"));

    ImmutableSet<APKModule> topLevelDeps = dag.getGraph().getOutgoingNodesFor(topModule);
    assertThat(topLevelDeps.size(), is(3));

    APKModule middleModule = null;
    for (APKModule apkModule : topLevelDeps) {
      assertThat(apkModule.getName(), oneOf(APKModule.ROOT_APKMODULE_NAME, "java", "java2"));
      if (apkModule.getName().equals("java")) {
        middleModule = apkModule;
      }
    }

    ImmutableSet<APKModule> middleLevelDeps = dag.getGraph().getOutgoingNodesFor(middleModule);
    assertThat(middleLevelDeps.size(), is(2));

    APKModule bottomModule = null;
    for (APKModule apkModule : middleLevelDeps) {
      assertThat(apkModule.getName(), oneOf(APKModule.ROOT_APKMODULE_NAME, "java2"));
      if (apkModule.getName().equals("java2")) {
        bottomModule = apkModule;
      }
    }

    ImmutableSet<APKModule> bottomLevelDeps = dag.getGraph().getOutgoingNodesFor(bottomModule);
    assertThat(bottomLevelDeps.size(), is(1));

    APKModule bottomDep = Iterables.getFirst(bottomLevelDeps, null);
    assertThat(bottomDep.getName(), is(APKModule.ROOT_APKMODULE_NAME));
  }

  /*
                   +----------------------------+
                   |                            |
                   |                            |
    +--------------+---------------+            |
    |              |               v            v
  +--------+     +---------+     +------+     +--------+     +--------+
  | Binary | --> | Android | --> | Java | --> | Shared | --> |        |
  +--------+     +---------+     +------+     +--------+     |        |
    |              |               |                         |        |
    +--------------+---------------+-----------------------> | Common |
                   |               |                         |        |
                   |               |                         |        |
                   +---------------+-----------------------> |        |
                                   |                         +--------+
                                   |                           ^
                                   +---------------------------+
  */

  @Test
  public void testAPKModuleGraphComplexDependencyTree() {
    ImmutableSet.Builder<ExternalTargetNode> nodeBuilder = ImmutableSet.builder();
    ExternalBuildTarget commonLibraryTarget = new ExternalBuildTarget("//:test-common-library");
    nodeBuilder.add(new ExternalTargetNode(commonLibraryTarget));

    ExternalBuildTarget sharedLibraryTarget = new ExternalBuildTarget("//:test-shared-library");
    nodeBuilder.add(new ExternalTargetNode(sharedLibraryTarget, Set.of(commonLibraryTarget)));

    ExternalBuildTarget javaDepLibraryTarget = new ExternalBuildTarget("//:test-java-dep-library");
    nodeBuilder.add(new ExternalTargetNode(javaDepLibraryTarget));

    ExternalBuildTarget javaLibraryTarget = new ExternalBuildTarget("//:test-java-library");
    nodeBuilder.add(
        new ExternalTargetNode(
            javaLibraryTarget,
            Set.of(commonLibraryTarget, javaDepLibraryTarget, sharedLibraryTarget)));

    ExternalBuildTarget androidLibraryTarget = new ExternalBuildTarget("//:test-android-library");
    nodeBuilder.add(
        new ExternalTargetNode(
            androidLibraryTarget,
            Set.of(javaLibraryTarget, commonLibraryTarget, sharedLibraryTarget)));

    ExternalBuildTarget keystoreTarget = new ExternalBuildTarget("//:keystore");
    nodeBuilder.add(new ExternalTargetNode(keystoreTarget));

    ExternalBuildTarget androidBinaryTarget = new ExternalBuildTarget("//:test-android-binary");
    nodeBuilder.add(
        new ExternalTargetNode(
            androidBinaryTarget,
            ImmutableSortedSet.of(androidLibraryTarget, javaLibraryTarget, commonLibraryTarget)));

    ExternalTargetGraph graph = buildGraph(nodeBuilder.build());

    ImmutableMap.Builder<String, ImmutableList<ExternalBuildTarget>> seedConfigMap =
        ImmutableMap.builder();
    seedConfigMap.put("android", ImmutableList.of(androidLibraryTarget));
    seedConfigMap.put("java", ImmutableList.of(javaLibraryTarget));

    ImmutableMap.Builder<String, ImmutableList<String>> appModuleDependencies =
        ImmutableMap.builder();
    appModuleDependencies.put("android", ImmutableList.of("java"));

    APKModuleGraph<ExternalBuildTarget> dag =
        new APKModuleGraph<>(
            graph,
            androidBinaryTarget,
            Optional.of(seedConfigMap.build()),
            Optional.of(appModuleDependencies.build()),
            Optional.empty());

    ImmutableSet<APKModule> topLevelNodes = dag.getGraph().getNodesWithNoIncomingEdges();
    assertThat(topLevelNodes.size(), is(1));

    for (APKModule apkModule : topLevelNodes) {
      assertThat(apkModule.getName(), equalTo("android"));
      ImmutableSet<APKModule> dependencies = dag.getGraph().getOutgoingNodesFor(apkModule);

      for (APKModule depModule : dependencies) {
        assertThat(depModule.getName(), oneOf("java", APKModule.ROOT_APKMODULE_NAME));
        switch (depModule.getName()) {
          case APKModule.ROOT_APKMODULE_NAME:
            assertThat(dag.getGraph().getOutgoingNodesFor(depModule).size(), is(0));
            break;
          case "java":
            verifyDependencies(dag, depModule, ImmutableSet.of(APKModule.ROOT_APKMODULE_NAME));
            break;
        }
      }
    }
  }
}
