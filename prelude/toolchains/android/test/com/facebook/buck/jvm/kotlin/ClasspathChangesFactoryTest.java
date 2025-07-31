/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin;

import static org.junit.Assert.assertTrue;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.jvm.java.ActionMetadata;
import com.facebook.buck.jvm.kotlin.kotlinc.incremental.ClasspathChanges;
import com.google.common.collect.ImmutableList;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;

public class ClasspathChangesFactoryTest {

  @Test
  public void when_jarFileIsAddedInCurrentDigest_then_ToBeComputedByIncrementalCompiler() {
    Path jarFile1 = Paths.get("lib/test1.jar");
    Path jarFile2 = Paths.get("lib/test2.jar");
    Path newJarFile = Paths.get("lib/new.jar");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(jarFile1, "jar1-digest");
    previousDigest.put(jarFile2, "jar2-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(jarFile1, "jar1-digest");
    currentDigest.put(jarFile2, "jar2-digest");
    currentDigest.put(newJarFile, "new-jar-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(jarsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void when_jarFileIsRemovedFromCurrentDigest_then_ToBeComputedByIncrementalCompiler() {
    Path jarFile1 = Paths.get("lib/test1.jar");
    Path jarFile2 = Paths.get("lib/test2.jar");
    Path removedJarFile = Paths.get("lib/removed.jar");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(jarFile1, "jar1-digest");
    previousDigest.put(jarFile2, "jar2-digest");
    previousDigest.put(removedJarFile, "removed-jar-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(jarFile1, "jar1-digest");
    currentDigest.put(jarFile2, "jar2-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(jarsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void when_jarFileIsModifiedInCurrentDigest_then_ToBeComputedByIncrementalCompiler() {
    Path jarFile1 = Paths.get("lib/test1.jar");
    Path jarFile2 = Paths.get("lib/test2.jar");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(jarFile1, "jar1-digest");
    previousDigest.put(jarFile2, "jar2-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(jarFile1, "jar1-digest-modified");
    currentDigest.put(jarFile2, "jar2-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(jarsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void when_multipleJarFilesAreModified_then_ToBeComputedByIncrementalCompiler() {
    Path jarFile1 = Paths.get("lib/test1.jar");
    Path jarFile2 = Paths.get("lib/test2.jar");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(jarFile1, "jar1-digest");
    previousDigest.put(jarFile2, "jar2-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(jarFile1, "jar1-digest-modified");
    currentDigest.put(jarFile2, "jar2-digest-modified");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(jarsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void when_noJarFilesAreChanged_then_NoChanges() {
    Path jarFile1 = Paths.get("lib/test1.jar");
    Path jarFile2 = Paths.get("lib/test2.jar");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(jarFile1, "jar1-digest");
    previousDigest.put(jarFile2, "jar2-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(jarFile1, "jar1-digest");
    currentDigest.put(jarFile2, "jar2-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(jarsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
  }

  @Test
  public void when_bothDigestsAreEmpty_then_NoChanges() {
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    Map<Path, String> currentDigest = new HashMap<>();
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(jarsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
  }

  @Test
  public void when_onlyNonJarFilesExist_then_NoChanges() {
    Path classFile = Paths.get("src/main/Test.class");
    Path javaFile = Paths.get("src/main/Test.java");
    Path txtFile = Paths.get("src/main/Test.txt");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(classFile, "class-digest");
    previousDigest.put(javaFile, "java-digest");
    previousDigest.put(txtFile, "txt-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(classFile, "class-digest-modified");
    currentDigest.put(javaFile, "java-digest-modified");
    currentDigest.put(txtFile, "txt-digest-modified");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(jarsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
  }

  @Test
  public void
      when_previousDigestIsEmptyAndCurrentContainsJars_then_ToBeComputedByIncrementalCompiler() {
    Path jarFile1 = Paths.get("lib/test1.jar");
    Path jarFile2 = Paths.get("lib/test2.jar");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(jarFile1, "jar1-digest");
    currentDigest.put(jarFile2, "jar2-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(jarsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void
      when_currentDigestIsEmptyAndPreviousContainsJars_then_ToBeComputedByIncrementalCompiler() {
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib/test1.jar"), "jar1-digest");
    previousDigest.put(Paths.get("lib/test2.jar"), "jar2-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(jarsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void when_jarFilesWithDifferentCaseExtensions_then_filtersCorrectly() {
    Path jarFile1 = Paths.get("lib/test1.jar");
    Path jarFile2 = Paths.get("lib/test2.JAR");
    Path jarFile3 = Paths.get("lib/test3.Jar");
    Path classFile = Paths.get("src/main/Test.class");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(jarFile1, "jar1-digest");
    previousDigest.put(jarFile2, "jar2-digest");
    previousDigest.put(jarFile3, "jar3-digest");
    previousDigest.put(classFile, "class-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(jarFile1, "jar1-digest");
    currentDigest.put(jarFile2, "jar2-digest");
    currentDigest.put(jarFile3, "jar3-digest");
    currentDigest.put(classFile, "class-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(jarsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
  }

  @Test
  public void when_nonJarFilesChangeButJarsRemainSame_then_NoChanges() {
    Path jarFile1 = Paths.get("lib/test1.jar");
    Path jarFile2 = Paths.get("lib/test2.jar");
    Path classFile = Paths.get("src/main/Test.class");
    Path javaFile = Paths.get("src/main/Test.java");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(jarFile1, "jar1-digest");
    previousDigest.put(jarFile2, "jar2-digest");
    previousDigest.put(classFile, "class-digest");
    previousDigest.put(javaFile, "java-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(jarFile1, "jar1-digest");
    currentDigest.put(jarFile2, "jar2-digest");
    currentDigest.put(classFile, "class-digest-modified");
    currentDigest.put(javaFile, "java-digest-modified");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(jarsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
  }

  @Test
  public void when_classpathSnapshotsProvided_then_includesSnapshotsInResult() {
    Path jarFile1 = Paths.get("lib/test1.jar");
    Path jarFile2 = Paths.get("lib/test2.jar");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(jarFile1, "jar1-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(jarFile1, "jar1-digest");
    currentDigest.put(jarFile2, "jar2-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    AbsPath snapshot1 = AbsPath.of(Paths.get("/path/to/snapshot1.jar"));
    AbsPath snapshot2 = AbsPath.of(Paths.get("/path/to/snapshot2.jar"));
    ImmutableList<AbsPath> classpathSnapshots = ImmutableList.of(snapshot1, snapshot2);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(jarsActionMetadata, classpathSnapshots);

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
    ClasspathChanges.ToBeComputedByIncrementalCompiler incrementalChanges =
        (ClasspathChanges.ToBeComputedByIncrementalCompiler) classpathChanges;
    assertTrue(incrementalChanges.getClasspathSnapshotFiles().contains(snapshot1.toFile()));
    assertTrue(incrementalChanges.getClasspathSnapshotFiles().contains(snapshot2.toFile()));
  }

  @Test
  public void when_noChangesWithClasspathSnapshots_then_includesSnapshotsInResult() {
    Path jarFile1 = Paths.get("lib/test1.jar");
    Path jarFile2 = Paths.get("lib/test2.jar");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(jarFile1, "jar1-digest");
    previousDigest.put(jarFile2, "jar2-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(jarFile1, "jar1-digest");
    currentDigest.put(jarFile2, "jar2-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    AbsPath snapshot1 = AbsPath.of(Paths.get("/path/to/snapshot1.jar"));
    AbsPath snapshot2 = AbsPath.of(Paths.get("/path/to/snapshot2.jar"));
    ImmutableList<AbsPath> classpathSnapshots = ImmutableList.of(snapshot1, snapshot2);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(jarsActionMetadata, classpathSnapshots);

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
    ClasspathChanges.NoChanges noChanges = (ClasspathChanges.NoChanges) classpathChanges;
    assertTrue(noChanges.getClasspathSnapshotFiles().contains(snapshot1.toFile()));
    assertTrue(noChanges.getClasspathSnapshotFiles().contains(snapshot2.toFile()));
  }
}
