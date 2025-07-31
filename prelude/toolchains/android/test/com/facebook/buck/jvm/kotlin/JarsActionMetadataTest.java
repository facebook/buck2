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

public class JarsActionMetadataTest {

  @Test
  public void when_constructorCalled_then_filtersOnlyJarFiles() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib1.jar"), "digest1");
    previousDigest.put(Paths.get("lib2.class"), "digest2");
    previousDigest.put(Paths.get("lib3.txt"), "digest3");
    previousDigest.put(Paths.get("lib4.jar"), "digest4");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.jar"), "digest1");
    currentDigest.put(Paths.get("lib2.class"), "digest2");
    currentDigest.put(Paths.get("lib3.txt"), "digest3");
    currentDigest.put(Paths.get("lib5.jar"), "digest5");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    assertEquals(2, jarsActionMetadata.getPreviousJarsDigest().size());
    assertEquals(2, jarsActionMetadata.getCurrentJarsDigest().size());
    assertTrue(jarsActionMetadata.getPreviousJarsDigest().containsKey(Paths.get("lib1.jar")));
    assertTrue(jarsActionMetadata.getPreviousJarsDigest().containsKey(Paths.get("lib4.jar")));
    assertFalse(jarsActionMetadata.getPreviousJarsDigest().containsKey(Paths.get("lib2.class")));
    assertFalse(jarsActionMetadata.getPreviousJarsDigest().containsKey(Paths.get("lib3.txt")));
    assertTrue(jarsActionMetadata.getCurrentJarsDigest().containsKey(Paths.get("lib1.jar")));
    assertTrue(jarsActionMetadata.getCurrentJarsDigest().containsKey(Paths.get("lib5.jar")));
    assertFalse(jarsActionMetadata.getCurrentJarsDigest().containsKey(Paths.get("lib2.class")));
    assertFalse(jarsActionMetadata.getCurrentJarsDigest().containsKey(Paths.get("lib3.txt")));
  }

  @Test
  public void when_noChanges_then_hasClasspathChangedReturnsFalse() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib2.jar"), "digest2");
    previousDigest.put(Paths.get("lib1.jar"), "digest1");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.jar"), "digest1");
    currentDigest.put(Paths.get("lib2.jar"), "digest2");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    assertFalse(jarsActionMetadata.hasClasspathChanged());
  }

  @Test
  public void when_digestChanged_then_hasClasspathChangedReturnsTrue() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib2.jar"), "digest2");
    previousDigest.put(Paths.get("lib1.jar"), "digest1");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.jar"), "digest1");
    currentDigest.put(Paths.get("lib2.jar"), "digest2_changed");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    assertTrue(jarsActionMetadata.hasClasspathChanged());
  }

  @Test
  public void when_newJarAdded_then_hasClasspathChangedReturnsTrue() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib1.jar"), "digest1");

    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.jar"), "digest1");
    currentDigest.put(Paths.get("lib2.jar"), "digest2");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    assertTrue(jarsActionMetadata.hasClasspathChanged());
  }

  @Test
  public void when_jarRemoved_then_hasClasspathChangedReturnsTrue() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib1.jar"), "digest1");
    previousDigest.put(Paths.get("lib2.jar"), "digest2");

    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib2.jar"), "digest2");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    assertTrue(jarsActionMetadata.hasClasspathChanged());
  }

  @Test
  public void when_emptyDigests_then_hasClasspathChangedReturnsFalse() {
    Map<Path, String> previousDigest = new HashMap<>();
    Map<Path, String> currentDigest = new HashMap<>();

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    assertFalse(jarsActionMetadata.hasClasspathChanged());
  }

  @Test
  public void when_multipleJarsChanged_then_hasClasspathChangedReturnsTrue() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib2.jar"), "digest2");
    previousDigest.put(Paths.get("lib1.jar"), "digest1");
    previousDigest.put(Paths.get("lib3.jar"), "digest3");
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.jar"), "digest1_changed");
    currentDigest.put(Paths.get("lib4.jar"), "digest4");
    currentDigest.put(Paths.get("lib2.jar"), "digest2");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    assertTrue(jarsActionMetadata.hasClasspathChanged());
  }

  @Test
  public void
      when_previousDigestEmptyAndCurrentDigestHasJars_then_hasClasspathChangedReturnsTrue() {
    Map<Path, String> previousDigest = new HashMap<>();
    Map<Path, String> currentDigest = new HashMap<>();
    currentDigest.put(Paths.get("lib1.jar"), "digest1");
    currentDigest.put(Paths.get("lib2.jar"), "digest2");

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    assertTrue(jarsActionMetadata.hasClasspathChanged());
  }

  @Test
  public void
      when_currentDigestEmptyAndPreviousDigestHasJars_then_hasClasspathChangedReturnsTrue() {
    Map<Path, String> previousDigest = new HashMap<>();
    previousDigest.put(Paths.get("lib1.jar"), "digest1");
    previousDigest.put(Paths.get("lib2.jar"), "digest2");
    Map<Path, String> currentDigest = new HashMap<>();

    ActionMetadata actionMetadata =
        new ActionMetadata(Paths.get("metadata.json"), previousDigest, currentDigest);
    JarsActionMetadata jarsActionMetadata = new JarsActionMetadata(actionMetadata);

    assertTrue(jarsActionMetadata.hasClasspathChanged());
  }
}
