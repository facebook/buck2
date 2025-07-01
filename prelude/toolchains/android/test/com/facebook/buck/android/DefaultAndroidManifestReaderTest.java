/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android;

import static org.junit.Assert.assertEquals;

import com.google.common.collect.ImmutableList;
import java.io.IOException;
import java.util.List;
import org.junit.Test;

public class DefaultAndroidManifestReaderTest {

  @Test
  public void testReadPackage() throws IOException {
    AndroidManifestReader manifestReader =
        DefaultAndroidManifestReader.forString(
            String.format(
                "<manifest xmlns:android='http://schemas.android.com/apk/res/android' package='%s'"
                    + " />",
                "com.example.package"));
    String packageName = manifestReader.getPackage();
    assertEquals("com.example.package", packageName);
  }

  @Test
  public void testReadVersionCode() throws IOException {
    AndroidManifestReader manifestReader =
        DefaultAndroidManifestReader.forString(
            "<manifest xmlns:android='http://schemas.android.com/apk/res/android' "
                + "android:versionCode=\"1\" />");
    String versionCode = manifestReader.getVersionCode();
    assertEquals("1", versionCode);
  }

  @Test
  public void testReadInstrumentationTestRunner() throws IOException {
    AndroidManifestReader manifestReader =
        DefaultAndroidManifestReader.forString(
            "<manifest xmlns:android='http://schemas.android.com/apk/res/android'>"
                + "  <instrumentation android:name='android.test.InstrumentationTestRunner' />"
                + "</manifest>");
    String instrumentationTestRunner = manifestReader.getInstrumentationTestRunner();
    assertEquals("android.test.InstrumentationTestRunner", instrumentationTestRunner);
  }

  @Test
  public void testReadLauncherActivitiesNoneFound() throws IOException {
    AndroidManifestReader manifestReader =
        DefaultAndroidManifestReader.forString(
            "<manifest xmlns:android='http://schemas.android.com/apk/res/android'>"
                + "  <application>"
                + "    <not-an-activity android:name='com.example.Activity6'>"
                + "      <intent-filter>"
                + "        <action android:name='android.intent.action.MAIN' />"
                + "        <category android:name='android.intent.category.LAUNCHER' />"
                + "      </intent-filter>"
                + "    </not-an-activity>"
                + "  </application>"
                + "</manifest>");
    List<String> found = manifestReader.getLauncherActivities();
    List<String> expected = ImmutableList.of();
    assertEquals(expected, found);
  }

  @Test
  public void testReadLauncherActivities() throws IOException {
    AndroidManifestReader manifestReader =
        DefaultAndroidManifestReader.forString(
            "<manifest xmlns:android='http://schemas.android.com/apk/res/android'"
                + "          package='com.example'>"
                + "  <application>"
                + "    <activity android:name='com.example.Activity1'>"
                + "      <intent-filter>"
                + "        <action android:name='android.intent.action.MAIN' />"
                + "        <category android:name='android.intent.category.LAUNCHER' />"
                + "      </intent-filter>"
                + "    </activity>"
                + "    <activity android:name='.Activity2'>"
                + "        <activity/> <!-- Make sure a weird manifest doesn't ruin things! -->"
                + "      <intent-filter>"
                + "        <category android:name='android.intent.category.LAUNCHER' />"
                + "        <action android:name='android.intent.action.MAIN' />"
                + "      </intent-filter>"
                + "    </activity>"
                + "    <activity android:name='com.example.Activity3'>"
                + "      <intent-filter>"
                + "        <category android:name='android.intent.category.LAUNCHER' />"
                + "      </intent-filter>"
                + "    </activity>"
                + "    <activity android:name='com.example.Activity4'>"
                + "      <intent-filter>"
                + "        <action android:name='android.intent.action.MAIN' />"
                + "      </intent-filter>"
                + "    </activity>"
                + "  </application>"
                + "</manifest>");
    List<String> found = manifestReader.getLauncherActivities();
    List<String> expected = ImmutableList.of("com.example.Activity1", ".Activity2");
    assertEquals(expected, found);
  }

  @Test
  public void testReadLauncherActivityAliases() throws IOException {
    AndroidManifestReader manifestReader =
        DefaultAndroidManifestReader.forString(
            "<manifest xmlns:android='http://schemas.android.com/apk/res/android'"
                + "          package='com.example'>"
                + "  <application>"
                + "    <activity-alias android:name='.ActivityAlias1' "
                + "        android:targetActivity='com.example.RealActivity1'>"
                + "      <intent-filter>"
                + "        <category android:name='android.intent.category.LAUNCHER' />"
                + "        <action android:name='android.intent.action.MAIN' />"
                + "      </intent-filter>"
                + "    </activity-alias>"
                + "  </application>"
                + "</manifest>");
    List<String> found = manifestReader.getLauncherActivities();
    List<String> expected = ImmutableList.of(".ActivityAlias1");
    assertEquals(expected, found);
  }

  @Test
  public void testReadLauncherActivitiesDisabled() throws IOException {
    AndroidManifestReader manifestReader =
        DefaultAndroidManifestReader.forString(
            "<manifest xmlns:android='http://schemas.android.com/apk/res/android'"
                + "          package='com.example'>"
                + "  <application>"
                + "    <activity android:name='.Activity1'>"
                + "      <intent-filter>"
                + "        <action android:name='android.intent.action.MAIN' />"
                + "        <category android:name='android.intent.category.LAUNCHER' />"
                + "      </intent-filter>"
                + "    </activity>"
                + "    <activity android:name='.Activity2' "
                + "        android:enabled='true'>"
                + "      <intent-filter>"
                + "        <action android:name='android.intent.action.MAIN' />"
                + "        <category android:name='android.intent.category.LAUNCHER' />"
                + "      </intent-filter>"
                + "    </activity>"
                + "    <activity android:name='.Activity3' "
                + "        android:enabled='false'>"
                + "      <intent-filter>"
                + "        <action android:name='android.intent.action.MAIN' />"
                + "        <category android:name='android.intent.category.LAUNCHER' />"
                + "      </intent-filter>"
                + "    </activity>"
                + "    <activity-alias android:name='.ActivityAlias1' "
                + "        android:targetActivity='.Activity1'>"
                + "      <intent-filter>"
                + "        <category android:name='android.intent.category.LAUNCHER' />"
                + "        <action android:name='android.intent.action.MAIN' />"
                + "      </intent-filter>"
                + "    </activity-alias>"
                + "    <activity-alias android:name='.ActivityAlias2' "
                + "        android:targetActivity='.Activity2' "
                + "        android:enabled='true'>"
                + "      <intent-filter>"
                + "        <category android:name='android.intent.category.LAUNCHER' />"
                + "        <action android:name='android.intent.action.MAIN' />"
                + "      </intent-filter>"
                + "    </activity-alias>"
                + "    <activity-alias android:name='.ActivityAlias3' "
                + "        android:targetActivity='.Activity3' "
                + "        android:enabled='false'>"
                + "      <intent-filter>"
                + "        <category android:name='android.intent.category.LAUNCHER' />"
                + "        <action android:name='android.intent.action.MAIN' />"
                + "      </intent-filter>"
                + "    </activity-alias>"
                + "  </application>"
                + "</manifest>");
    List<String> found = manifestReader.getLauncherActivities();
    List<String> expected =
        ImmutableList.of(".Activity1", ".Activity2", ".ActivityAlias1", ".ActivityAlias2");
    assertEquals(expected, found);
  }
}
