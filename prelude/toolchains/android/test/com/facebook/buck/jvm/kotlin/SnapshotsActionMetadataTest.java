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
import java.util.Map;
import org.junit.Test;

public class SnapshotsActionMetadataTest {

  @Test
  public void when_constructorCalled_then_filtersOnlyJarFiles() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib1.bin"), "digest1");
    previousDigest.put(Paths.get("lib2.class"), "digest2");
    previousDigest.put(Paths.get("lib3.txt"), "digest3");
    previousDigest.put(Paths.get("lib4.bin"), "digest4");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.bin"), "digest1");
    currentDigest.put(Paths.get("lib2.class"), "digest2");
    currentDigest.put(Paths.get("lib3.txt"), "digest3");
    currentDigest.put(Paths.get("lib5.bin"), "digest5");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertEquals(2, snapshotsActionMetadata.getPreviousSnapshotsDigest().size());
    assertEquals(2, snapshotsActionMetadata.getCurrentSnapshotsDigest().size());
    assertTrue(
        snapshotsActionMetadata.getPreviousSnapshotsDigest().containsKey(Paths.get("lib1.bin")));
    assertTrue(
        snapshotsActionMetadata.getPreviousSnapshotsDigest().containsKey(Paths.get("lib4.bin")));
    assertFalse(
        snapshotsActionMetadata.getPreviousSnapshotsDigest().containsKey(Paths.get("lib2.class")));
    assertFalse(
        snapshotsActionMetadata.getPreviousSnapshotsDigest().containsKey(Paths.get("lib3.txt")));
    assertTrue(
        snapshotsActionMetadata.getCurrentSnapshotsDigest().containsKey(Paths.get("lib1.bin")));
    assertTrue(
        snapshotsActionMetadata.getCurrentSnapshotsDigest().containsKey(Paths.get("lib5.bin")));
    assertFalse(
        snapshotsActionMetadata.getCurrentSnapshotsDigest().containsKey(Paths.get("lib2.class")));
    assertFalse(
        snapshotsActionMetadata.getCurrentSnapshotsDigest().containsKey(Paths.get("lib3.txt")));
  }

  @Test
  public void when_noChanges_then_hasClasspathChangedReturnsFalse() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib2.bin"), "digest2");
    previousDigest.put(Paths.get("lib1.bin"), "digest1");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.bin"), "digest1");
    currentDigest.put(Paths.get("lib2.bin"), "digest2");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertFalse(snapshotsActionMetadata.hasClasspathChanged());
  }

  @Test
  public void when_digestChanged_then_hasClasspathChangedReturnsTrue() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib2.bin"), "digest2");
    previousDigest.put(Paths.get("lib1.bin"), "digest1");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.bin"), "digest1");
    currentDigest.put(Paths.get("lib2.bin"), "digest2_changed");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertTrue(snapshotsActionMetadata.hasClasspathChanged());
  }

  @Test
  public void when_newJarAdded_then_hasClasspathChangedReturnsTrue() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib1.bin"), "digest1");

    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.bin"), "digest1");
    currentDigest.put(Paths.get("lib2.bin"), "digest2");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertTrue(snapshotsActionMetadata.hasClasspathChanged());
  }

  @Test
  public void when_jarRemoved_then_hasClasspathChangedReturnsTrue() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib1.bin"), "digest1");
    previousDigest.put(Paths.get("lib2.bin"), "digest2");

    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib2.bin"), "digest2");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertTrue(snapshotsActionMetadata.hasClasspathChanged());
  }

  @Test
  public void when_emptyDigests_then_hasClasspathChangedReturnsFalse() {
    Map<Path, String> previousDigest = new HashMap<>();
    Map<Path, String> currentDigest = new HashMap<>();

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertFalse(snapshotsActionMetadata.hasClasspathChanged());
  }

  @Test
  public void when_multipleSnapshotsChanged_then_hasClasspathChangedReturnsTrue() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib2.bin"), "digest2");
    previousDigest.put(Paths.get("lib1.bin"), "digest1");
    previousDigest.put(Paths.get("lib3.bin"), "digest3");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.bin"), "digest1_changed");
    currentDigest.put(Paths.get("lib4.bin"), "digest4");
    currentDigest.put(Paths.get("lib2.bin"), "digest2");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertTrue(snapshotsActionMetadata.hasClasspathChanged());
  }

  @Test
  public void
      when_previousDigestEmptyAndCurrentDigestHasSnapshots_then_hasClasspathChangedReturnsTrue() {
    Map<Path, String> previousDigest = new HashMap<>();
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.bin"), "digest1");
    currentDigest.put(Paths.get("lib2.bin"), "digest2");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertTrue(snapshotsActionMetadata.hasClasspathChanged());
  }

  @Test
  public void
      when_currentDigestEmptyAndPreviousDigestHasSnapshots_then_hasClasspathChangedReturnsTrue() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib1.bin"), "digest1");
    previousDigest.put(Paths.get("lib2.bin"), "digest2");
    Map<Path, String> currentDigest = new HashMap<>();

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertTrue(snapshotsActionMetadata.hasClasspathChanged());
  }

  @Test
  public void when_jarRemoved_then_hasClasspathRemovalReturnsTrue() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib1.bin"), "digest1");
    previousDigest.put(Paths.get("lib2.bin"), "digest2");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib2.bin"), "digest2");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertTrue(snapshotsActionMetadata.hasClasspathRemoval());
  }

  @Test
  public void when_allJarsRemoved_then_hasClasspathRemovalReturnsTrue() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib1.bin"), "digest1");
    previousDigest.put(Paths.get("lib2.bin"), "digest2");
    Map<Path, String> currentDigest = new HashMap<>();

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertTrue(snapshotsActionMetadata.hasClasspathRemoval());
  }

  @Test
  public void when_jarAdded_then_hasClasspathRemovalReturnsFalse() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib1.bin"), "digest1");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.bin"), "digest1");
    currentDigest.put(Paths.get("lib2.bin"), "digest2");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertFalse(snapshotsActionMetadata.hasClasspathRemoval());
  }

  @Test
  public void when_jarModified_then_hasClasspathRemovalReturnsFalse() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib1.bin"), "digest1");
    previousDigest.put(Paths.get("lib2.bin"), "digest2");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.bin"), "digest1_changed");
    currentDigest.put(Paths.get("lib2.bin"), "digest2");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertFalse(snapshotsActionMetadata.hasClasspathRemoval());
  }

  @Test
  public void when_noChanges_then_hasClasspathRemovalReturnsFalse() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib1.bin"), "digest1");
    previousDigest.put(Paths.get("lib2.bin"), "digest2");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.bin"), "digest1");
    currentDigest.put(Paths.get("lib2.bin"), "digest2");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertFalse(snapshotsActionMetadata.hasClasspathRemoval());
  }

  @Test
  public void when_emptyDigests_then_hasClasspathRemovalReturnsFalse() {
    Map<Path, String> previousDigest = new HashMap<>();
    Map<Path, String> currentDigest = new HashMap<>();

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertFalse(snapshotsActionMetadata.hasClasspathRemoval());
  }

  @Test
  public void when_jarAddedAndRemoved_then_hasClasspathRemovalReturnsTrue() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib1.bin"), "digest1");
    previousDigest.put(Paths.get("lib2.bin"), "digest2");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.bin"), "digest1");
    currentDigest.put(Paths.get("lib3.bin"), "digest3");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertTrue(snapshotsActionMetadata.hasClasspathRemoval());
  }

  @Test
  public void when_contentBasedPathChanges_then_hasClasspathRemovalReturnsFalse() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(
        Paths.get("gen/pkg/__target__/__action__/abc123def456789a/foo_snapshot.bin"), "digest1");
    previousDigest.put(
        Paths.get("gen/pkg/__target__/__action__/abc123def456789a/bar_snapshot.bin"), "digest2");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(
        Paths.get("gen/pkg/__target__/__action__/fedcba9876543210/foo_snapshot.bin"),
        "digest1_new");
    currentDigest.put(
        Paths.get("gen/pkg/__target__/__action__/fedcba9876543210/bar_snapshot.bin"),
        "digest2_new");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertFalse(snapshotsActionMetadata.hasClasspathRemoval());
  }

  @Test
  public void when_sameFilenameButDifferentPackage_removalDetected() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(
        Paths.get("gen/pkgA/__model__/__action__/aaaaaaaaaaaaaaaa/model_snapshot.bin"), "digest1");
    previousDigest.put(
        Paths.get("gen/pkgB/__model__/__action__/bbbbbbbbbbbbbbbb/model_snapshot.bin"), "digest2");
    Map<Path, String> currentDigest = new HashMap<>();
    // pkgA/:model removed, only pkgB/:model remains (with new content hash)
    currentDigest.put(
        Paths.get("gen/pkgB/__model__/__action__/cccccccccccccccc/model_snapshot.bin"),
        "digest2_new");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    SnapshotsActionMetadata snapshotsActionMetadata = new SnapshotsActionMetadata(actionMetadata);

    assertTrue(snapshotsActionMetadata.hasClasspathRemoval());
  }
}
