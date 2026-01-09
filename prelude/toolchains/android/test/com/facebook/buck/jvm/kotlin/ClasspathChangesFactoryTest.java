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
  public void when_snapshotFileIsAddedInCurrentDigest_then_ToBeComputedByIncrementalCompiler() {
    Path snapshotFile1 = Paths.get("lib/test1.bin");
    Path snapshotFile2 = Paths.get("lib/test2.bin");
    Path newSnapshotFile = Paths.get("lib/new.bin");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(snapshotFile1, "snapshot1-digest");
    previousDigest.put(snapshotFile2, "snapshot2-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(snapshotFile1, "snapshot1-digest");
    currentDigest.put(snapshotFile2, "snapshot2-digest");
    currentDigest.put(newSnapshotFile, "new-snapshot-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(snapshotsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void when_snapshotFileIsRemovedFromCurrentDigest_then_HasRemovals() {
    Path snapshotFile1 = Paths.get("lib/test1.bin");
    Path snapshotFile2 = Paths.get("lib/test2.bin");
    Path removedSnapshotFile = Paths.get("lib/removed.bin");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(snapshotFile1, "snapshot1-digest");
    previousDigest.put(snapshotFile2, "snapshot2-digest");
    previousDigest.put(removedSnapshotFile, "removed-snapshot-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(snapshotFile1, "snapshot1-digest");
    currentDigest.put(snapshotFile2, "snapshot2-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(snapshotsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.HasRemovals);
  }

  @Test
  public void when_snapshotFileIsModifiedInCurrentDigest_then_ToBeComputedByIncrementalCompiler() {
    Path snapshotFile1 = Paths.get("lib/test1.bin");
    Path snapshotFile2 = Paths.get("lib/test2.bin");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(snapshotFile1, "snapshot1-digest");
    previousDigest.put(snapshotFile2, "snapshot2-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(snapshotFile1, "snapshot1-digest-modified");
    currentDigest.put(snapshotFile2, "snapshot2-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(snapshotsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void when_multipleSnapshotFilesAreModified_then_ToBeComputedByIncrementalCompiler() {
    Path snapshotFile1 = Paths.get("lib/test1.bin");
    Path snapshotFile2 = Paths.get("lib/test2.bin");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(snapshotFile1, "snapshot1-digest");
    previousDigest.put(snapshotFile2, "snapshot2-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(snapshotFile1, "snapshot1-digest-modified");
    currentDigest.put(snapshotFile2, "snapshot2-digest-modified");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(snapshotsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void when_noSnapshotFilesAreChanged_then_NoChanges() {
    Path snapshotFile1 = Paths.get("lib/test1.bin");
    Path snapshotFile2 = Paths.get("lib/test2.bin");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(snapshotFile1, "snapshot1-digest");
    previousDigest.put(snapshotFile2, "snapshot2-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(snapshotFile1, "snapshot1-digest");
    currentDigest.put(snapshotFile2, "snapshot2-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(snapshotsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
  }

  @Test
  public void when_bothDigestsAreEmpty_then_NoChanges() {
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    Map<Path, String> currentDigest = new HashMap<>();
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(snapshotsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
  }

  @Test
  public void when_onlyNonSnapshotFilesExist_then_NoChanges() {
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
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(snapshotsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
  }

  @Test
  public void
      when_previousDigestIsEmptyAndCurrentContainsSnapshots_then_ToBeComputedByIncrementalCompiler() {
    Path snapshotFile1 = Paths.get("lib/test1.bin");
    Path snapshotFile2 = Paths.get("lib/test2.bin");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(snapshotFile1, "snapshot1-digest");
    currentDigest.put(snapshotFile2, "snapshot2-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(snapshotsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
  }

  @Test
  public void when_currentDigestIsEmptyAndPreviousContainsSnapshots_then_HasRemovals() {
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib/test1.bin"), "snapshot1-digest");
    previousDigest.put(Paths.get("lib/test2.bin"), "snapshot2-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(snapshotsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.HasRemovals);
  }

  @Test
  public void when_snapshotFilesWithDifferentCaseExtensions_then_filtersCorrectly() {
    Path snapshotFile1 = Paths.get("lib/test1.bin");
    Path snapshotFile2 = Paths.get("lib/test2.BIN");
    Path snapshotFile3 = Paths.get("lib/test3.Snapshot");
    Path classFile = Paths.get("src/main/Test.class");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(snapshotFile1, "snapshot1-digest");
    previousDigest.put(snapshotFile2, "snapshot2-digest");
    previousDigest.put(snapshotFile3, "snapshot3-digest");
    previousDigest.put(classFile, "class-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(snapshotFile1, "snapshot1-digest");
    currentDigest.put(snapshotFile2, "snapshot2-digest");
    currentDigest.put(snapshotFile3, "snapshot3-digest");
    currentDigest.put(classFile, "class-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(snapshotsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
  }

  @Test
  public void when_nonSnapshotFilesChangeButSnapshotsRemainSame_then_NoChanges() {
    Path snapshotFile1 = Paths.get("lib/test1.bin");
    Path snapshotFile2 = Paths.get("lib/test2.bin");
    Path classFile = Paths.get("src/main/Test.class");
    Path javaFile = Paths.get("src/main/Test.java");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(snapshotFile1, "snapshot1-digest");
    previousDigest.put(snapshotFile2, "snapshot2-digest");
    previousDigest.put(classFile, "class-digest");
    previousDigest.put(javaFile, "java-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(snapshotFile1, "snapshot1-digest");
    currentDigest.put(snapshotFile2, "snapshot2-digest");
    currentDigest.put(classFile, "class-digest-modified");
    currentDigest.put(javaFile, "java-digest-modified");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(snapshotsActionMetadata, ImmutableList.of());

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
  }

  @Test
  public void when_classpathSnapshotsProvided_then_includesSnapshotsInResult() {
    Path snapshotFile1 = Paths.get("lib/test1.bin");
    Path snapshotFile2 = Paths.get("lib/test2.bin");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(snapshotFile1, "snapshot1-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(snapshotFile1, "snapshot1-digest");
    currentDigest.put(snapshotFile2, "snapshot2-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    AbsPath snapshot1 = AbsPath.of(Paths.get("/path/to/snapshot1.bin"));
    AbsPath snapshot2 = AbsPath.of(Paths.get("/path/to/snapshot2.bin"));
    ImmutableList<AbsPath> classpathSnapshots = ImmutableList.of(snapshot1, snapshot2);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(snapshotsActionMetadata, classpathSnapshots);

    assertTrue(classpathChanges instanceof ClasspathChanges.ToBeComputedByIncrementalCompiler);
    ClasspathChanges.ToBeComputedByIncrementalCompiler incrementalChanges =
        (ClasspathChanges.ToBeComputedByIncrementalCompiler) classpathChanges;
    assertTrue(incrementalChanges.getClasspathSnapshotFiles().contains(snapshot1.toFile()));
    assertTrue(incrementalChanges.getClasspathSnapshotFiles().contains(snapshot2.toFile()));
  }

  @Test
  public void when_noChangesWithClasspathSnapshots_then_includesSnapshotsInResult() {
    Path snapshotFile1 = Paths.get("lib/test1.bin");
    Path snapshotFile2 = Paths.get("lib/test2.bin");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(snapshotFile1, "snapshot1-digest");
    previousDigest.put(snapshotFile2, "snapshot2-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(snapshotFile1, "snapshot1-digest");
    currentDigest.put(snapshotFile2, "snapshot2-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    AbsPath snapshot1 = AbsPath.of(Paths.get("/path/to/snapshot1.bin"));
    AbsPath snapshot2 = AbsPath.of(Paths.get("/path/to/snapshot2.bin"));
    ImmutableList<AbsPath> classpathSnapshots = ImmutableList.of(snapshot1, snapshot2);

    ClasspathChanges classpathChanges =
        ClasspathChangesFactory.create(snapshotsActionMetadata, classpathSnapshots);

    assertTrue(classpathChanges instanceof ClasspathChanges.NoChanges);
    ClasspathChanges.NoChanges noChanges = (ClasspathChanges.NoChanges) classpathChanges;
    assertTrue(noChanges.getClasspathSnapshotFiles().contains(snapshot1.toFile()));
    assertTrue(noChanges.getClasspathSnapshotFiles().contains(snapshot2.toFile()));
  }
}
