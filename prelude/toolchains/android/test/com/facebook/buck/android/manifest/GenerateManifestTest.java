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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import com.android.utils.ILogger;
import com.android.utils.NullLogger;
import com.google.common.collect.ImmutableMap;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/** Test manifest placeholder replacement */
public class GenerateManifestTest {

  private static final boolean SANITY_CHECK_ENABLED = true;
  private static final boolean SANITY_CHECK_DISABLED = false;

  @Rule public TemporaryFolder tempFolder = new TemporaryFolder();

  private static final ILogger LOGGER = NullLogger.getLogger();
  private static final PreprocessLogger PREPROCESS_LOGGER;

  static {
    try {
      PREPROCESS_LOGGER = PreprocessLogger.create(null, LOGGER);
    } catch (IOException e) {
      throw new RuntimeException("Failed to create PreprocessLogger", e);
    }
  }

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

  @Test(expected = RuntimeException.class)
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

  @Test
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

    String replaced =
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
    assertEquals(expected, replaced);
  }

  @Test
  public void shouldMaintainManifestAsIsWhenActivityAliasHasCorrectOrder() {
    String goodManifest =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest\n"
            + "   xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "   xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "   package=\"com.package.name\">\n"
            + "   <application>\n"
            + "       <activity android:name=\"TestActivity\" />\n"
            + "       <activity-alias\n"
            + "           android:name=\"JustATestAlias\"\n"
            + "           android:targetActivity=\"TestActivity\" />\n"
            + "   </application>\n"
            + "</manifest>";

    String sorted = GenerateManifest.moveActivityAliasesToEnd(goodManifest);

    assertEquals(goodManifest, sorted);
  }

  @Test
  public void shouldMoveActivityAliasAfterItsTargetActivityWhenOrderIsIncorrect() {
    String unsortedManifest =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "    package=\"com.package.name\" >\n"
            + "    <application>\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAlias\"\n"
            + "            android:targetActivity=\"TestActivity\" />\n"
            + "        <activity android:name=\"TestActivity\" />\n"
            + "    </application>\n"
            + "</manifest>";

    String sorted = GenerateManifest.moveActivityAliasesToEnd(unsortedManifest);

    String expected =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "    package=\"com.package.name\" >\n"
            + "    <application>\n"
            + "        <activity android:name=\"TestActivity\" />\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAlias\"\n"
            + "            android:targetActivity=\"TestActivity\" />\n"
            + "    </application>\n"
            + "</manifest>";
    assertEquals(expected, sorted);
  }

  @Test
  public void
      shouldMoveUnsortedActivityAliasAtTheEndOfTheFileWhenWrongAliasIsBeforeCorrectlySortedAlias() {
    String unsortedManifest =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "    package=\"com.package.name\" >\n"
            + "    <application>\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasOne\"\n"
            + "            android:targetActivity=\"TestActivityOne\" />\n"
            + "        <activity android:name=\"TestActivityOne\" />\n"
            + "        <activity android:name=\"TestActivityTwo\" />\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasTwo\"\n"
            + "            android:targetActivity=\"TestActivityTwo\" />\n"
            + "    </application>\n"
            + "</manifest>";

    String sorted = GenerateManifest.moveActivityAliasesToEnd(unsortedManifest);

    String expected =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "    package=\"com.package.name\" >\n"
            + "    <application>\n"
            + "        <activity android:name=\"TestActivityOne\" />\n"
            + "        <activity android:name=\"TestActivityTwo\" />\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasTwo\"\n"
            + "            android:targetActivity=\"TestActivityTwo\" />\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasOne\"\n"
            + "            android:targetActivity=\"TestActivityOne\" />\n"
            + "    </application>\n"
            + "</manifest>";
    assertEquals(expected, sorted);
  }

  @Test
  public void
      shouldMoveUnsortedActivityAliasAtTheEndOfTheFileWhenWrongAliasIsAfterCorrectlySortedAlias() {
    String unsortedManifest =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "    package=\"com.package.name\" >\n"
            + "    <application>\n"
            + "        <activity android:name=\"TestActivityOne\" />\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasOne\"\n"
            + "            android:targetActivity=\"TestActivityOne\" />\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasTwo\"\n"
            + "            android:targetActivity=\"TestActivityTwo\" />\n"
            + "        <activity android:name=\"TestActivityTwo\" />\n"
            + "    </application>\n"
            + "</manifest>";

    String sorted = GenerateManifest.moveActivityAliasesToEnd(unsortedManifest);

    String expected =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "    package=\"com.package.name\" >\n"
            + "    <application>\n"
            + "        <activity android:name=\"TestActivityOne\" />\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasOne\"\n"
            + "            android:targetActivity=\"TestActivityOne\" />\n"
            + "        <activity android:name=\"TestActivityTwo\" />\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasTwo\"\n"
            + "            android:targetActivity=\"TestActivityTwo\" />\n"
            + "    </application>\n"
            + "</manifest>";
    assertEquals(expected, sorted);
  }

  @Test
  public void shouldMoveUnsortedActivityAliasesAtTheEndWhenMultipleAreUnsortedAndBatched() {
    String unsortedManifest =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "    package=\"com.package.name\" >\n"
            + "    <application>\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasOne\"\n"
            + "            android:targetActivity=\"TestActivityOne\" />\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasTwo\"\n"
            + "            android:targetActivity=\"TestActivityTwo\" />\n"
            + "        <activity android:name=\"TestActivityOne\" />\n"
            + "        <activity android:name=\"TestActivityTwo\" />\n"
            + "    </application>\n"
            + "</manifest>";

    String sorted = GenerateManifest.moveActivityAliasesToEnd(unsortedManifest);

    String expected =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "    package=\"com.package.name\" >\n"
            + "    <application>\n"
            + "        <activity android:name=\"TestActivityOne\" />\n"
            + "        <activity android:name=\"TestActivityTwo\" />\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasOne\"\n"
            + "            android:targetActivity=\"TestActivityOne\" />\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasTwo\"\n"
            + "            android:targetActivity=\"TestActivityTwo\" />\n"
            + "    </application>\n"
            + "</manifest>";
    assertEquals(expected, sorted);
  }

  @Test
  public void shouldMoveUnsortedActivityAliasesAtTheEndWhenMultipleAreUnsortedAndAlternated() {
    String unsortedManifest =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "    package=\"com.package.name\" >\n"
            + "    <application>\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasOne\"\n"
            + "            android:targetActivity=\"TestActivityOne\" />\n"
            + "        <activity android:name=\"TestActivityOne\" />\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasTwo\"\n"
            + "            android:targetActivity=\"TestActivityTwo\" />\n"
            + "        <activity android:name=\"TestActivityTwo\" />\n"
            + "    </application>\n"
            + "</manifest>";

    String sorted = GenerateManifest.moveActivityAliasesToEnd(unsortedManifest);

    String expected =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    xmlns:tools=\"http://schemas.android.com/tools\"\n"
            + "    package=\"com.package.name\" >\n"
            + "    <application>\n"
            + "        <activity android:name=\"TestActivityOne\" />\n"
            + "        <activity android:name=\"TestActivityTwo\" />\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasOne\"\n"
            + "            android:targetActivity=\"TestActivityOne\" />\n"
            + "        <activity-alias\n"
            + "            android:name=\"JustATestAliasTwo\"\n"
            + "            android:targetActivity=\"TestActivityTwo\" />\n"
            + "    </application>\n"
            + "</manifest>";
    assertEquals(expected, sorted);
  }

  @Test
  public void shouldExtractBothSdkVersionsFromManifest() throws Exception {
    File manifest = tempFolder.newFile("AndroidManifest.xml");
    String manifestContent =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    package=\"com.example.app\">\n"
            + "    <uses-sdk\n"
            + "        android:minSdkVersion=\"21\"\n"
            + "        android:targetSdkVersion=\"33\" />\n"
            + "</manifest>";
    Files.write(manifest.toPath(), manifestContent.getBytes());

    GenerateManifest.SdkVersions versions = GenerateManifest.extractSdkVersions(manifest, LOGGER);

    assertEquals("21", versions.getMinSdkVersion());
    assertEquals("33", versions.getTargetSdkVersion());
  }

  @Test
  public void shouldExtractOnlyMinSdkVersionWhenTargetSdkVersionIsMissing() throws Exception {
    File manifest = tempFolder.newFile("AndroidManifest.xml");
    String manifestContent =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    package=\"com.example.app\">\n"
            + "    <uses-sdk android:minSdkVersion=\"21\" />\n"
            + "</manifest>";
    Files.write(manifest.toPath(), manifestContent.getBytes());

    GenerateManifest.SdkVersions versions = GenerateManifest.extractSdkVersions(manifest, LOGGER);

    assertEquals("21", versions.getMinSdkVersion());
    assertNull(versions.getTargetSdkVersion());
  }

  @Test
  public void shouldExtractOnlyTargetSdkVersionWhenMinSdkVersionIsMissing() throws Exception {
    File manifest = tempFolder.newFile("AndroidManifest.xml");
    String manifestContent =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    package=\"com.example.app\">\n"
            + "    <uses-sdk android:targetSdkVersion=\"33\" />\n"
            + "</manifest>";
    Files.write(manifest.toPath(), manifestContent.getBytes());

    GenerateManifest.SdkVersions versions = GenerateManifest.extractSdkVersions(manifest, LOGGER);

    assertNull(versions.getMinSdkVersion());
    assertEquals("33", versions.getTargetSdkVersion());
  }

  @Test
  public void shouldReturnNullVersionsWhenUsesSdkElementIsMissing() throws Exception {
    File manifest = tempFolder.newFile("AndroidManifest.xml");
    String manifestContent =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    package=\"com.example.app\">\n"
            + "</manifest>";
    Files.write(manifest.toPath(), manifestContent.getBytes());

    GenerateManifest.SdkVersions versions = GenerateManifest.extractSdkVersions(manifest, LOGGER);

    assertNull(versions.getMinSdkVersion());
    assertNull(versions.getTargetSdkVersion());
  }

  @Test
  public void shouldInjectBothSdkVersionsIntoLibraryManifestWhenMissing() throws Exception {
    File libraryManifest = tempFolder.newFile("library_manifest.xml");
    String manifestContent =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    package=\"com.example.library\">\n"
            + "</manifest>";
    Files.write(libraryManifest.toPath(), manifestContent.getBytes());

    Path outputDir = tempFolder.newFolder("output").toPath();
    GenerateManifest.SdkVersions versions = new GenerateManifest.SdkVersions("21", "33");

    File processed =
        GenerateManifest.preprocessLibraryManifest(
            libraryManifest, outputDir, versions, PREPROCESS_LOGGER, LOGGER);

    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware(true);
    DocumentBuilder builder = factory.newDocumentBuilder();
    Document doc = builder.parse(processed);

    NodeList usesSdkNodes = doc.getElementsByTagName("uses-sdk");
    assertEquals(1, usesSdkNodes.getLength());

    Element usesSdk = (Element) usesSdkNodes.item(0);
    assertEquals("21", usesSdk.getAttributeNS(GenerateManifest.ANDROID_NAMESPACE, "minSdkVersion"));
    assertEquals(
        "33", usesSdk.getAttributeNS(GenerateManifest.ANDROID_NAMESPACE, "targetSdkVersion"));
  }

  @Test
  public void shouldInjectOnlyTargetSdkVersionWhenMinSdkVersionAlreadyExists() throws Exception {
    File libraryManifest = tempFolder.newFile("library_manifest.xml");
    String manifestContent =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    package=\"com.example.library\">\n"
            + "    <uses-sdk android:minSdkVersion=\"19\" />\n"
            + "</manifest>";
    Files.write(libraryManifest.toPath(), manifestContent.getBytes());

    Path outputDir = tempFolder.newFolder("output").toPath();
    GenerateManifest.SdkVersions versions = new GenerateManifest.SdkVersions("21", "33");

    File processed =
        GenerateManifest.preprocessLibraryManifest(
            libraryManifest, outputDir, versions, PREPROCESS_LOGGER, LOGGER);

    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware(true);
    DocumentBuilder builder = factory.newDocumentBuilder();
    Document doc = builder.parse(processed);

    NodeList usesSdkNodes = doc.getElementsByTagName("uses-sdk");
    assertEquals(1, usesSdkNodes.getLength());

    Element usesSdk = (Element) usesSdkNodes.item(0);
    assertEquals("19", usesSdk.getAttributeNS(GenerateManifest.ANDROID_NAMESPACE, "minSdkVersion"));
    assertEquals(
        "33", usesSdk.getAttributeNS(GenerateManifest.ANDROID_NAMESPACE, "targetSdkVersion"));
  }

  @Test
  public void shouldNotModifyLibraryManifestWhenBothSdkVersionsAlreadyExist() throws Exception {
    File libraryManifest = tempFolder.newFile("library_manifest.xml");
    String manifestContent =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    package=\"com.example.library\">\n"
            + "    <uses-sdk\n"
            + "        android:minSdkVersion=\"19\"\n"
            + "        android:targetSdkVersion=\"30\" />\n"
            + "</manifest>";
    Files.write(libraryManifest.toPath(), manifestContent.getBytes());

    Path outputDir = tempFolder.newFolder("output").toPath();
    GenerateManifest.SdkVersions versions = new GenerateManifest.SdkVersions("21", "33");

    File processed =
        GenerateManifest.preprocessLibraryManifest(
            libraryManifest, outputDir, versions, PREPROCESS_LOGGER, LOGGER);

    assertEquals(libraryManifest, processed);
  }

  @Test
  public void shouldNotModifyLibraryManifestWhenSdkVersionsAreNull() throws Exception {
    File libraryManifest = tempFolder.newFile("library_manifest.xml");
    String manifestContent =
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            + "<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n"
            + "    package=\"com.example.library\">\n"
            + "</manifest>";
    Files.write(libraryManifest.toPath(), manifestContent.getBytes());

    Path outputDir = tempFolder.newFolder("output").toPath();
    GenerateManifest.SdkVersions versions = new GenerateManifest.SdkVersions(null, null);

    File processed =
        GenerateManifest.preprocessLibraryManifest(
            libraryManifest, outputDir, versions, PREPROCESS_LOGGER, LOGGER);

    assertEquals(libraryManifest, processed);
  }

  @Test
  public void shouldEnsureAndroidNamespaceWhenMissing() throws Exception {
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware(true);
    DocumentBuilder builder = factory.newDocumentBuilder();
    Document doc = builder.newDocument();

    Element manifest = doc.createElement("manifest");
    doc.appendChild(manifest);

    GenerateManifest.ensureAndroidNamespace(doc);

    assertEquals(GenerateManifest.ANDROID_NAMESPACE, manifest.getAttribute("xmlns:android"));
  }

  @Test
  public void shouldNotModifyAndroidNamespaceWhenAlreadyPresent() throws Exception {
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware(true);
    DocumentBuilder builder = factory.newDocumentBuilder();
    Document doc = builder.newDocument();

    Element manifest = doc.createElement("manifest");
    manifest.setAttribute("xmlns:android", GenerateManifest.ANDROID_NAMESPACE);
    doc.appendChild(manifest);

    GenerateManifest.ensureAndroidNamespace(doc);

    assertEquals(GenerateManifest.ANDROID_NAMESPACE, manifest.getAttribute("xmlns:android"));
  }

  @Test
  public void shouldReturnExistingUsesSdkElement() throws Exception {
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware(true);
    DocumentBuilder builder = factory.newDocumentBuilder();
    Document doc = builder.newDocument();

    Element manifest = doc.createElement("manifest");
    doc.appendChild(manifest);

    Element existingUsesSdk = doc.createElement("uses-sdk");
    existingUsesSdk.setAttributeNS(
        GenerateManifest.ANDROID_NAMESPACE, "android:minSdkVersion", "21");
    manifest.appendChild(existingUsesSdk);

    Element result = GenerateManifest.getOrCreateUsesSdk(doc);

    assertEquals(existingUsesSdk, result);
    assertEquals("21", result.getAttributeNS(GenerateManifest.ANDROID_NAMESPACE, "minSdkVersion"));
  }

  @Test
  public void shouldCreateNewUsesSdkElementWhenMissing() throws Exception {
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware(true);
    DocumentBuilder builder = factory.newDocumentBuilder();
    Document doc = builder.newDocument();

    Element manifest = doc.createElement("manifest");
    doc.appendChild(manifest);

    Element result = GenerateManifest.getOrCreateUsesSdk(doc);

    assertNotNull(result);
    assertEquals("uses-sdk", result.getTagName());

    NodeList usesSdkNodes = doc.getElementsByTagName("uses-sdk");
    assertEquals(1, usesSdkNodes.getLength());
  }

  @Test
  public void shouldInsertUsesSdkElementAsFirstChildWhenManifestHasChildren() throws Exception {
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    factory.setNamespaceAware(true);
    DocumentBuilder builder = factory.newDocumentBuilder();
    Document doc = builder.newDocument();

    Element manifest = doc.createElement("manifest");
    doc.appendChild(manifest);

    Element application = doc.createElement("application");
    manifest.appendChild(application);

    Element result = GenerateManifest.getOrCreateUsesSdk(doc);

    assertNotNull(result);
    assertEquals("uses-sdk", result.getTagName());
    assertEquals(result, manifest.getFirstChild());
  }
}
