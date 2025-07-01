/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.manifest;

import static org.junit.Assert.assertEquals;

import com.facebook.buck.core.exceptions.HumanReadableException;
import com.google.common.collect.ImmutableMap;
import java.io.IOException;
import org.junit.Test;

/** Test manifest placeholder replacement */
public class GenerateManifestTest {

  private static final boolean SANITY_CHECK_ENABLED = true;
  private static final boolean SANITY_CHECK_DISABLED = false;

  @Test
  public void shouldReplaceManifestPlaceholders() {
    String placeholderText =
        "<manifest>\n"
            + "    <permission\n"
            + "        android:name=\"${applicationId}.permission.C2D_MESSAGE\"\n"
            + "        android:protectionLevel=\"${custom$}\" />\n"
            + "</manifest>";
    ImmutableMap<String, String> placeholders =
        ImmutableMap.of("applicationId", "com.example", "custom$", "0x2");
    String replaced = GenerateManifest.replacePlaceholders(placeholderText, placeholders);
    String expected =
        "<manifest>\n"
            + "    <permission\n"
            + "        android:name=\"com.example.permission.C2D_MESSAGE\"\n"
            + "        android:protectionLevel=\"0x2\" />\n"
            + "</manifest>";

    assertEquals(expected, replaced);
  }

  @Test
  public void shouldReplaceApplicationIdFromPackageName() throws IOException {
    String goodManifest =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest\n"
            + "   xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "   xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "   package=\"com.package.name\">\n"
            + "   <permission android:name=\"${applicationId}.permission.C2D_MESSAGE\"\n />"
            + "</manifest>";

    String replaced =
        GenerateManifest.replaceApplicationIdPlaceholders(goodManifest, SANITY_CHECK_DISABLED);

    String expected =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest\n"
            + "   xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "   xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "   package=\"com.package.name\">\n"
            + "   <permission android:name=\"com.package.name.permission.C2D_MESSAGE\"\n />"
            + "</manifest>";
    assertEquals(expected, replaced);
  }

  @Test
  public void
      shouldReplaceApplicationIdFromPackageNameAndNotThrownForMultiLineCommentsWhenVerificationEnabled()
          throws IOException {
    String goodManifest =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest\n"
            + "   xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "   xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "   package=\"com.package.name\">\n"
            + "   <permission android:name=\"${applicationId}.permission.C2D_MESSAGE\"\n />"
            + "   <!--\n"
            + "       ${invalid_placeholder}\n"
            + "   -->\n"
            + "</manifest>";

    String replaced =
        GenerateManifest.replaceApplicationIdPlaceholders(goodManifest, SANITY_CHECK_ENABLED);

    String expected =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest\n"
            + "   xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "   xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "   package=\"com.package.name\">\n"
            + "   <permission android:name=\"com.package.name.permission.C2D_MESSAGE\"\n />"
            + "   <!--\n"
            + "       ${invalid_placeholder}\n"
            + "   -->\n"
            + "</manifest>";
    assertEquals(expected, replaced);
  }

  @Test
  public void
      shouldReplaceApplicationIdFromPackageNameAndNotThrownForSinglelineCommentsWhenVerificationEnabled()
          throws IOException {
    String goodManifest =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest\n"
            + "   xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "   xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "   package=\"com.package.name\">\n"
            + "   <permission android:name=\"${applicationId}.permission.C2D_MESSAGE\"\n />"
            + "   <!-- ${invalid_placeholder} -->\n"
            + "</manifest>";

    String replaced =
        GenerateManifest.replaceApplicationIdPlaceholders(goodManifest, SANITY_CHECK_ENABLED);

    String expected =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest\n"
            + "   xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "   xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "   package=\"com.package.name\">\n"
            + "   <permission android:name=\"com.package.name.permission.C2D_MESSAGE\"\n />"
            + "   <!-- ${invalid_placeholder} -->\n"
            + "</manifest>";
    assertEquals(expected, replaced);
  }

  @Test
  public void
      shouldReplaceApplicationIdFromPackageNameAndNotThrownForSinglelineNoSpaceCommentsWhenVerificationEnabled()
          throws IOException {
    String goodManifest =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest\n"
            + "   xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "   xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "   package=\"com.package.name\">\n"
            + "   <permission android:name=\"${applicationId}.permission.C2D_MESSAGE\"\n />"
            + "   <!--${invalid_placeholder}-->\n"
            + "</manifest>";

    String replaced =
        GenerateManifest.replaceApplicationIdPlaceholders(goodManifest, SANITY_CHECK_ENABLED);

    String expected =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest\n"
            + "   xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "   xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "   package=\"com.package.name\">\n"
            + "   <permission android:name=\"com.package.name.permission.C2D_MESSAGE\"\n />"
            + "   <!--${invalid_placeholder}-->\n"
            + "</manifest>";
    assertEquals(expected, replaced);
  }

  @Test
  public void
      shouldReplaceApplicationIdFromPackageNameAndNotThrownForInlinedCommentsWhenVerificationEnabled()
          throws IOException {
    String goodManifest =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest\n"
            + "   xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "   xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "   package=\"com.package.name\">\n"
            + "   <permission android:name=\"${applicationId}.permission.C2D_MESSAGE\"\n />"
            + "<!-- ${invalid_placeholder} --></manifest>";

    String replaced =
        GenerateManifest.replaceApplicationIdPlaceholders(goodManifest, SANITY_CHECK_ENABLED);

    String expected =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest\n"
            + "   xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "   xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "   package=\"com.package.name\">\n"
            + "   <permission android:name=\"com.package.name.permission.C2D_MESSAGE\"\n />"
            + "<!-- ${invalid_placeholder} --></manifest>";
    assertEquals(expected, replaced);
  }

  @Test(expected = HumanReadableException.class)
  public void shouldThrowExceptionWhenThereArePlaceholdersNotReplacedWhenVerificationEnabled()
      throws IOException {
    String manifestWithUnreplacedPlaceholders =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest\n"
            + "   xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "   xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "   package=\"com.package.name\">\n"
            + "   <permission\n"
            + "       android:name=\"${applicationId}.permission.C2D_MESSAGE\"\n"
            + "       android:protectionLevel=\"${unreplacedPlaceholder}\" />\n"
            + "</manifest>";

    GenerateManifest.replaceApplicationIdPlaceholders(
        manifestWithUnreplacedPlaceholders, SANITY_CHECK_ENABLED);
  }

  public void shouldNotThrowExceptionWhenThereArePlaceholdersNotReplacedWhenVerificationDisabled()
      throws IOException {
    String manifestWithUnreplacedPlaceholders =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest\n"
            + "   xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "   xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "   package=\"com.package.name\">\n"
            + "   <permission\n"
            + "       android:name=\"${applicationId}.permission.C2D_MESSAGE\"\n"
            + "       android:protectionLevel=\"${unreplacedPlaceholder}\" />\n"
            + "</manifest>";

    GenerateManifest.replaceApplicationIdPlaceholders(
        manifestWithUnreplacedPlaceholders, SANITY_CHECK_DISABLED);

    String expected =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest\n"
            + "   xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "   xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "   package=\"com.package.name\">\n"
            + "   <permission\n"
            + "       android:name=\"com.package.name.permission.C2D_MESSAGE\"\n"
            + "       android:protectionLevel=\"${unreplacedPlaceholder}\" />\n"
            + "</manifest>";
    assertEquals(expected, expected);
  }
}
