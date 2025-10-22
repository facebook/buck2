/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.resources;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.google.common.collect.ImmutableSet;
import java.util.Optional;
import java.util.regex.Pattern;
import org.junit.Test;

public class MergeAssetsUtilsTest {

  // Extensions found in a codehub search for uses of extra_no_compress_asset_extensions
  private static ImmutableSet<String> extraExtensions =
      ImmutableSet.of("zip", "dict", "spo", "srt", "txt");

  // ==================== Tests for Simple Extensions (.jpg, .wav) ====================

  @Test
  public void testNormalExtensions() {

    // Combine with DEFAULT_NO_COMPRESS_EXTENSIONS
    ImmutableSet<String> allExtensions =
        new ImmutableSet.Builder<String>()
            .addAll(MergeAssetsUtils.DEFAULT_NO_COMPRESS_EXTENSIONS)
            .addAll(extraExtensions)
            .build();

    // Test each of DEFAULT_NO_COMPRESS_EXTENSIONS
    for (String ext : MergeAssetsUtils.DEFAULT_NO_COMPRESS_EXTENSIONS) {
      testExtension(ext, allExtensions);
    }

    // Test each of extraExtensions
    for (String ext : extraExtensions) {
      testExtension(ext, allExtensions);
    }
  }

  private void testExtension(String ext, ImmutableSet<String> allExtensions) {
    // Match in assets folder
    assertTrue(
        MergeAssetsUtils.isNoCompress("assets/file." + ext, allExtensions, Optional.empty()));
    // Match in nested folder
    assertTrue(
        MergeAssetsUtils.isNoCompress(
            "assets/foo/bar/file." + ext, allExtensions, Optional.empty()));
    // Match in nested folder with . in the directory path
    assertTrue(
        MergeAssetsUtils.isNoCompress(
            "assets/foo/v.20/file." + ext, allExtensions, Optional.empty()));
    // No match on folders with ext name
    assertFalse(
        MergeAssetsUtils.isNoCompress(
            "assets/" + ext + "/foo.bin", allExtensions, Optional.empty()));
    // Match files starting with a different extension
    assertTrue(
        MergeAssetsUtils.isNoCompress("assets/file.bin." + ext, allExtensions, Optional.empty()));
    // No match on files ending with a different extension
    assertFalse(
        MergeAssetsUtils.isNoCompress(
            "assets/file." + ext + ".bin", allExtensions, Optional.empty()));
    // No match on similar extensions that start with the extension
    assertFalse(
        MergeAssetsUtils.isNoCompress("assets/file." + ext + "s", allExtensions, Optional.empty()));
    // No match on similar extensions that end with the extension
    assertFalse(
        MergeAssetsUtils.isNoCompress("assets/file.x" + ext, allExtensions, Optional.empty()));
    // No match if file name and ext aren't at the end of the path
    assertFalse(
        MergeAssetsUtils.isNoCompress(
            "assets/file." + ext + "/foo", allExtensions, Optional.empty()));
    // No match if file name doesn't have a dot
    assertFalse(MergeAssetsUtils.isNoCompress("assets/" + ext, allExtensions, Optional.empty()));
    // Match is case-sensitive (.JPG shouldn't match for .jpg)
    String allcapExt = ext.toUpperCase();
    if (!allcapExt.equals(ext)) {
      assertFalse(
          MergeAssetsUtils.isNoCompress(
              "assets/file." + allcapExt, allExtensions, Optional.empty()));
    }
  }

  // ==================== Tests for Regex ====================

  @Test
  public void testRegex() {
    String regex =
        "worldtemplatemain\\.bank/bank$|cybersectormain\\.bank/bank$|fpstestworldfirepit\\.bank/bank$";
    Optional<Pattern> pattern = Optional.of(Pattern.compile(regex));

    assertTrue(
        MergeAssetsUtils.isNoCompress(
            "assets/meta/audio/worldtemplatemain.bank/bank", ImmutableSet.of(), pattern));
    assertFalse(
        MergeAssetsUtils.isNoCompress(
            "assets/meta/audio/worldtemplatemain.strings.bank/bank", ImmutableSet.of(), pattern));
    assertTrue(
        MergeAssetsUtils.isNoCompress(
            "assets/meta/cyber_sector/usd/assets/cybersectormain.bank/bank",
            ImmutableSet.of(),
            pattern));
    assertFalse(
        MergeAssetsUtils.isNoCompress(
            "assets/meta/cyber_sector/usd/assets/cybersectormain.strings.bank/bank",
            ImmutableSet.of(),
            pattern));
    assertTrue(
        MergeAssetsUtils.isNoCompress(
            "assets/meta/fps_test_world/audio/fpstestworldfirepit.bank/bank",
            ImmutableSet.of(),
            pattern));
    assertFalse(
        MergeAssetsUtils.isNoCompress(
            "assets/meta/fps_test_world/audio/fpstestworldmain.strings.bank/bank",
            ImmutableSet.of(),
            pattern));
  }
}
