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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.facebook.buck.jvm.java.ActionMetadata;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.Test;

public class SourceFilesActionMetadataTest {

  @Test
  public void
      when_previousDigestContainsSourceFiles_then_previousSourceFilesDigestFiltersCorrectly() {
    Path javaFile = Paths.get("src/main/Test.java");
    Path ktFile = Paths.get("src/main/Test.kt");
    Path otherFile = Paths.get("src/main/Test.txt");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(javaFile, "java-digest");
    previousDigest.put(ktFile, "kt-digest");
    previousDigest.put(otherFile, "other-digest");
    previousDigest.put(metadataFile, "metadata-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SourceFilesActionMetadata sourceFilesActionMetadata =
        new SourceFilesActionMetadata(actionMetadata);

    Map<Path, String> result = sourceFilesActionMetadata.getPreviousSourceFilesDigest();

    assertEquals(2, result.size());
    assertEquals("java-digest", result.get(javaFile));
    assertEquals("kt-digest", result.get(ktFile));
    assertFalse(result.containsKey(otherFile));
    assertFalse(result.containsKey(metadataFile));
  }

  @Test
  public void
      when_currentDigestContainsSourceFiles_then_currentSourceFilesDigestFiltersCorrectly() {
    Path javaFile = Paths.get("src/main/Test.java");
    Path ktFile = Paths.get("src/main/Test.kt");
    Path otherFile = Paths.get("src/main/Test.txt");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(javaFile, "java-digest");
    currentDigest.put(ktFile, "kt-digest");
    currentDigest.put(otherFile, "other-digest");
    currentDigest.put(metadataFile, "metadata-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SourceFilesActionMetadata sourceFilesActionMetadata =
        new SourceFilesActionMetadata(actionMetadata);

    Map<Path, String> result = sourceFilesActionMetadata.getCurrentSourceFilesDigest();

    assertEquals(2, result.size());
    assertEquals("java-digest", result.get(javaFile));
    assertEquals("kt-digest", result.get(ktFile));
    assertFalse(result.containsKey(otherFile));
    assertFalse(result.containsKey(metadataFile));
  }

  @Test
  public void
      when_fileIsAddedInCurrentDigest_then_calculateAddedAndModifiedSourceFilesIncludesIt() {
    Path javaFile = Paths.get("src/main/Test.java");
    Path ktFile = Paths.get("src/main/Test.kt");
    Path newFile = Paths.get("src/main/NewFile.java");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(javaFile, "java-digest");
    previousDigest.put(ktFile, "kt-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(javaFile, "java-digest");
    currentDigest.put(ktFile, "kt-digest");
    currentDigest.put(newFile, "new-file-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SourceFilesActionMetadata sourceFilesActionMetadata =
        new SourceFilesActionMetadata(actionMetadata);

    List<Path> result = sourceFilesActionMetadata.calculateAddedAndModifiedSourceFiles();

    assertEquals(1, result.size());
    assertTrue(result.contains(newFile));
  }

  @Test
  public void
      when_fileIsModifiedInCurrentDigest_then_calculateAddedAndModifiedSourceFilesIncludesIt() {
    Path javaFile = Paths.get("src/main/Test.java");
    Path ktFile = Paths.get("src/main/Test.kt");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(javaFile, "java-digest");
    previousDigest.put(ktFile, "kt-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(javaFile, "java-digest-modified");
    currentDigest.put(ktFile, "kt-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SourceFilesActionMetadata sourceFilesActionMetadata =
        new SourceFilesActionMetadata(actionMetadata);

    List<Path> result = sourceFilesActionMetadata.calculateAddedAndModifiedSourceFiles();

    assertEquals(1, result.size());
    assertTrue(result.contains(javaFile));
  }

  @Test
  public void
      when_multipleFilesAreAddedAndModified_then_calculateAddedAndModifiedSourceFilesIncludesAll() {
    Path javaFile = Paths.get("src/main/Test.java");
    Path ktFile = Paths.get("src/main/Test.kt");
    Path newJavaFile = Paths.get("src/main/NewTest.java");
    Path newKtFile = Paths.get("src/main/NewTest.kt");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(javaFile, "java-digest");
    previousDigest.put(ktFile, "kt-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(javaFile, "java-digest-modified");
    currentDigest.put(ktFile, "kt-digest");
    currentDigest.put(newJavaFile, "new-java-digest");
    currentDigest.put(newKtFile, "new-kt-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SourceFilesActionMetadata sourceFilesActionMetadata =
        new SourceFilesActionMetadata(actionMetadata);

    List<Path> result = sourceFilesActionMetadata.calculateAddedAndModifiedSourceFiles();

    assertEquals(3, result.size());
    assertTrue(result.contains(javaFile));
    assertTrue(result.contains(newJavaFile));
    assertTrue(result.contains(newKtFile));
    assertFalse(result.contains(ktFile)); // unchanged file should not be included
  }

  @Test
  public void
      when_noFilesAreAddedOrModified_then_calculateAddedAndModifiedSourceFilesReturnsEmpty() {
    Path javaFile = Paths.get("src/main/Test.java");
    Path ktFile = Paths.get("src/main/Test.kt");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(javaFile, "java-digest");
    previousDigest.put(ktFile, "kt-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(javaFile, "java-digest");
    currentDigest.put(ktFile, "kt-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SourceFilesActionMetadata sourceFilesActionMetadata =
        new SourceFilesActionMetadata(actionMetadata);

    List<Path> result = sourceFilesActionMetadata.calculateAddedAndModifiedSourceFiles();

    assertTrue(result.isEmpty());
  }

  @Test
  public void when_fileIsRemovedFromCurrentDigest_then_calculateRemovedFilesIncludesIt() {
    Path javaFile = Paths.get("src/main/Test.java");
    Path ktFile = Paths.get("src/main/Test.kt");
    Path removedFile = Paths.get("src/main/RemovedFile.java");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(javaFile, "java-digest");
    previousDigest.put(ktFile, "kt-digest");
    previousDigest.put(removedFile, "removed-file-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(javaFile, "java-digest");
    currentDigest.put(ktFile, "kt-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SourceFilesActionMetadata sourceFilesActionMetadata =
        new SourceFilesActionMetadata(actionMetadata);

    List<Path> result = sourceFilesActionMetadata.calculateRemovedFiles();

    assertEquals(1, result.size());
    assertTrue(result.contains(removedFile));
  }

  @Test
  public void when_multipleFilesAreRemoved_then_calculateRemovedFilesIncludesAll() {
    Path javaFile = Paths.get("src/main/Test.java");
    Path ktFile = Paths.get("src/main/Test.kt");
    Path removedJavaFile = Paths.get("src/main/RemovedTest.java");
    Path removedKtFile = Paths.get("src/main/RemovedTest.kt");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(javaFile, "java-digest");
    previousDigest.put(ktFile, "kt-digest");
    previousDigest.put(removedJavaFile, "removed-java-digest");
    previousDigest.put(removedKtFile, "removed-kt-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(javaFile, "java-digest");
    currentDigest.put(ktFile, "kt-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SourceFilesActionMetadata sourceFilesActionMetadata =
        new SourceFilesActionMetadata(actionMetadata);

    List<Path> result = sourceFilesActionMetadata.calculateRemovedFiles();

    assertEquals(2, result.size());
    assertTrue(result.contains(removedJavaFile));
    assertTrue(result.contains(removedKtFile));
    assertFalse(result.contains(javaFile)); // existing file should not be included
    assertFalse(result.contains(ktFile)); // existing file should not be included
  }

  @Test
  public void when_noFilesAreRemoved_then_calculateRemovedFilesReturnsEmpty() {
    Path javaFile = Paths.get("src/main/Test.java");
    Path ktFile = Paths.get("src/main/Test.kt");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(javaFile, "java-digest");
    previousDigest.put(ktFile, "kt-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(javaFile, "java-digest");
    currentDigest.put(ktFile, "kt-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SourceFilesActionMetadata sourceFilesActionMetadata =
        new SourceFilesActionMetadata(actionMetadata);

    List<Path> result = sourceFilesActionMetadata.calculateRemovedFiles();

    assertTrue(result.isEmpty());
  }

  @Test
  public void when_previousDigestIsEmpty_then_calculateRemovedFilesReturnsEmpty() {
    Path javaFile = Paths.get("src/main/Test.java");
    Path ktFile = Paths.get("src/main/Test.kt");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(javaFile, "java-digest");
    currentDigest.put(ktFile, "kt-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SourceFilesActionMetadata sourceFilesActionMetadata =
        new SourceFilesActionMetadata(actionMetadata);

    List<Path> result = sourceFilesActionMetadata.calculateRemovedFiles();

    assertTrue(result.isEmpty());
  }

  @Test
  public void when_currentDigestIsEmpty_then_calculateAddedAndModifiedSourceFilesReturnsEmpty() {
    Path javaFile = Paths.get("src/main/Test.java");
    Path ktFile = Paths.get("src/main/Test.kt");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(javaFile, "java-digest");
    previousDigest.put(ktFile, "kt-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);
    SourceFilesActionMetadata sourceFilesActionMetadata =
        new SourceFilesActionMetadata(actionMetadata);

    List<Path> result = sourceFilesActionMetadata.calculateAddedAndModifiedSourceFiles();

    assertTrue(result.isEmpty());
  }

  @Test
  public void when_onlyNonSourceFilesExist_then_sourceFilesDigestsAreEmpty() {
    Path txtFile = Paths.get("src/main/Test.txt");
    Path xmlFile = Paths.get("src/main/config.xml");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(txtFile, "txt-digest");
    previousDigest.put(xmlFile, "xml-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(txtFile, "txt-digest");
    currentDigest.put(xmlFile, "xml-digest");
    ActionMetadata actionMetadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);

    SourceFilesActionMetadata sourceFilesActionMetadata =
        new SourceFilesActionMetadata(actionMetadata);

    assertTrue(sourceFilesActionMetadata.getPreviousSourceFilesDigest().isEmpty());
    assertTrue(sourceFilesActionMetadata.getCurrentSourceFilesDigest().isEmpty());
    assertTrue(sourceFilesActionMetadata.calculateAddedAndModifiedSourceFiles().isEmpty());
    assertTrue(sourceFilesActionMetadata.calculateRemovedFiles().isEmpty());
  }
}
