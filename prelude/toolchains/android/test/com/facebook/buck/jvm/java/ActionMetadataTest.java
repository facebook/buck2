/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;

public class ActionMetadataTest {

  @Test
  public void
      when_previousDigestContainsSourceFiles_then_getPreviousSourceFilesDigestFiltersCorrectly() {
    Path javaFile = Paths.get("src/main/Test.java");
    Path ktFile = Paths.get("src/main/Test.kt");
    Path otherFile = Paths.get("src/main/Test.txt");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(javaFile, "java-digest");
    previousDigest.put(ktFile, "kt-digest");
    previousDigest.put(otherFile, "other-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    ActionMetadata metadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);

    Map<Path, String> result = metadata.getPreviousSourceFilesDigest();

    assertEquals(2, result.size());
    assertEquals("java-digest", result.get(javaFile));
    assertEquals("kt-digest", result.get(ktFile));
    assertFalse(result.containsKey(otherFile));
  }

  @Test
  public void
      when_currentDigestContainsSourceFiles_then_getCurrentSourceFilesDigestFiltersCorrectly() {
    Path javaFile = Paths.get("src/main/Test.java");
    Path ktFile = Paths.get("src/main/Test.kt");
    Path otherFile = Paths.get("src/main/Test.txt");
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(javaFile, "java-digest");
    currentDigest.put(ktFile, "kt-digest");
    currentDigest.put(otherFile, "other-digest");
    ActionMetadata metadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);

    Map<Path, String> result = metadata.getCurrentSourceFilesDigest();

    assertEquals(2, result.size());
    assertEquals("java-digest", result.get(javaFile));
    assertEquals("kt-digest", result.get(ktFile));
    assertFalse(result.containsKey(otherFile));
  }

  @Test
  public void
      when_previousDigestContainsMetadataFile_then_getPreviousIncrementalMetadataDigestReturnsCorrectValue() {
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(metadataFile, "metadata-digest");
    previousDigest.put(Paths.get("other.file"), "other-digest");
    Map<Path, String> currentDigest = new HashMap<>();
    ActionMetadata metadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);

    String previousIncrementalMetadataDigest = metadata.getPreviousIncrementalMetadataDigest();

    assertEquals("metadata-digest", previousIncrementalMetadataDigest);
  }

  @Test
  public void
      when_currentDigestContainsMetadataFile_then_getCurrentIncrementalMetadataDigestReturnsCorrectValue() {
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(metadataFile, "metadata-digest");
    currentDigest.put(Paths.get("other.file"), "other-digest");
    ActionMetadata metadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);

    String currentIncrementalMetadataDigest = metadata.getCurrentIncrementalMetadataDigest();

    assertEquals("metadata-digest", currentIncrementalMetadataDigest);
  }

  @Test
  public void when_digestsDoNotContainMetadataFile_then_incrementalMetadataDigestReturnsNull() {
    Path metadataFile = Paths.get("metadata.json");
    Map<Path, String> previousDigest = new HashMap<>();
    Map<Path, String> currentDigest = new HashMap<>();
    ActionMetadata metadata = new ActionMetadata(metadataFile, previousDigest, currentDigest);

    String previousIncrementalMetadataDigest = metadata.getPreviousIncrementalMetadataDigest();
    String currentIncrementalMetadataDigest = metadata.getCurrentIncrementalMetadataDigest();

    assertNull(previousIncrementalMetadataDigest);
    assertNull(currentIncrementalMetadataDigest);
  }
}
