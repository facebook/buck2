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
import static org.junit.Assert.assertFalse;

import com.facebook.buck.android.exopackage.AdbCommandFailedException;
import com.facebook.buck.android.exopackage.ExopackageInstaller;
import com.facebook.buck.android.exopackage.ExopackageUtil;
import com.facebook.buck.android.exopackage.IsolatedExopackageInfo;
import com.facebook.buck.android.exopackage.PackageInfo;
import com.facebook.buck.android.exopackage.SetDebugAppMode;
import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.testutil.TemporaryPaths;
import com.google.common.base.Strings;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSortedSet;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeSet;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

/** When an exopackage install stops the app, relative to the device changes it makes. */
public class ExopackageInstallerStopTest {

  private static final String PACKAGE = "com.example.app";
  private static final String HASH = Strings.repeat("a", 40);
  private static final Path DEX_FILE = Paths.get("secondary-dex/secondary-" + HASH + ".dex.jar");
  private static final Path METADATA = Paths.get("secondary-dex/metadata.txt");
  private static final String METADATA_CONTENTS =
      "secondary-" + HASH + ".dex.jar " + HASH + " secondary.dex01.Canary\n";

  @Rule public final TemporaryPaths tmpDir = new TemporaryPaths();

  private RecordingDevice device;
  private IsolatedExopackageInfo exoInfo;
  private IsolatedApkInfo apkInfo;
  private String localApkHash;

  @Before
  public void setUp() throws Exception {
    AbsPath root = tmpDir.getRoot();
    Path dexDir = Files.createDirectories(root.resolve("dex").getPath());
    Files.write(dexDir.resolve("secondary-1.dex.jar"), new byte[] {1, 2, 3});
    Path dexMetadata =
        Files.write(
            dexDir.resolve("metadata.txt"),
            ("secondary-1.dex.jar " + HASH + " secondary.dex01.Canary\n")
                .getBytes(StandardCharsets.UTF_8));
    exoInfo =
        new IsolatedExopackageInfo(
            Optional.of(
                new IsolatedExopackageInfo.IsolatedDexInfo(
                    AbsPath.of(dexMetadata), AbsPath.of(dexDir))),
            Optional.empty(),
            Optional.empty());

    File apk = root.resolve("app.apk").toFile();
    Files.write(apk.toPath(), new byte[] {4, 5, 6});
    apkInfo = new IsolatedApkInfo(root.resolve("AndroidManifest.xml"), AbsPath.of(apk.toPath()));
    localApkHash = ExopackageUtil.getContentHash(apk);

    device = new RecordingDevice();
  }

  @Test
  public void stopsBeforeDeletingStaleFiles() throws Exception {
    device.files.add(DEX_FILE);
    device.files.add(Paths.get("secondary-dex/secondary-" + Strings.repeat("b", 40) + ".dex.jar"));
    device.metadata(METADATA_CONTENTS);
    device.installedApkHash = localApkHash;

    install();

    assertEquals(ImmutableList.of("stop", "delete"), device.events());
  }

  @Test
  public void stopsBeforeRewritingChangedMetadata() throws Exception {
    // The new dex file was already pushed while the build ran, so only the metadata changes.
    device.files.add(DEX_FILE);
    device.metadata("secondary-old.dex.jar " + Strings.repeat("c", 40) + " old\n");
    device.installedApkHash = localApkHash;

    install();

    assertEquals(ImmutableList.of("stop", "push:metadata"), device.events());
  }

  @Test
  public void pushesNewFilesWhileTheAppIsStillRunning() throws Exception {
    device.installedApkHash = localApkHash;

    install();

    assertEquals(ImmutableList.of("push:secondary_dex", "stop", "push:metadata"), device.events());
  }

  @Test
  public void stopsBeforeWritingMissingMetadata() throws Exception {
    // A previous install pushed the dex file and stopped before writing metadata.
    device.files.add(DEX_FILE);
    device.installedApkHash = localApkHash;

    install();

    assertEquals(ImmutableList.of("stop", "push:metadata"), device.events());
  }

  @Test
  public void leavesTheAppRunningWhenNothingChanged() throws Exception {
    device.files.add(DEX_FILE);
    device.metadata(METADATA_CONTENTS);
    device.installedApkHash = localApkHash;

    install();

    assertEquals(ImmutableList.of(), device.events());
  }

  @Test
  public void restartsAnUnchangedAppWhenAnActivityWillBeStarted() throws Exception {
    device.files.add(DEX_FILE);
    device.metadata(METADATA_CONTENTS);
    device.installedApkHash = localApkHash;

    install(/* restartApp= */ true);

    assertEquals(ImmutableList.of("stop"), device.events());
  }

  @Test
  public void replacingTheApkNeedsNoStopOfItsOwn() throws Exception {
    device.files.add(DEX_FILE);
    device.metadata(METADATA_CONTENTS);
    device.installedApkHash = "an older apk";

    install(/* restartApp= */ true);

    assertEquals(ImmutableList.of("install"), device.events());
  }

  @Test
  public void leavesUnchangedMetadataAlone() throws Exception {
    device.files.add(DEX_FILE);
    device.metadata(METADATA_CONTENTS);
    device.installedApkHash = localApkHash;

    install();

    assertFalse(device.events().contains("push:metadata"));
  }

  private void install() throws Exception {
    install(/* restartApp= */ false);
  }

  private void install(boolean restartApp) throws Exception {
    new ExopackageInstaller(
            exoInfo, new NoopPrinter(), tmpDir.getRoot(), PACKAGE, device, Optional.empty())
        .doInstall(apkInfo, SetDebugAppMode.SKIP, restartApp);
  }

  /** Records, in order, every operation an app running on the device could notice. */
  private static class RecordingDevice extends TestAndroidDevice {
    private final List<String> events = Collections.synchronizedList(new ArrayList<>());
    final TreeSet<Path> files = new TreeSet<>();
    private final Map<String, String> hashes = new HashMap<>();
    String installedApkHash = "";

    void metadata(String text) {
      files.add(METADATA);
      hashes.put(
          ExopackageInstaller.EXOPACKAGE_INSTALL_ROOT.resolve(PACKAGE).resolve(METADATA).toString(),
          ExopackageUtil.getContentHash(text.getBytes(StandardCharsets.UTF_8)));
    }

    ImmutableList<String> events() {
      synchronized (events) {
        return ImmutableList.copyOf(events);
      }
    }

    @Override
    public void stopPackage(String packageName) {
      events.add("stop");
    }

    @Override
    public void rmFiles(String dirPath, Iterable<String> filesToDelete) {
      events.add("delete");
    }

    @Override
    public void installFiles(String filesType, Map<Path, Path> installPaths, String packageName) {
      events.add("push:" + filesType);
    }

    @Override
    public boolean installApkOnDevice(
        File apk,
        boolean installViaSd,
        boolean quiet,
        boolean verifyTempWritable,
        boolean stagedInstallMode,
        String userId,
        boolean allowFastDeploy,
        String packageName) {
      events.add("install");
      return true;
    }

    @Override
    public Optional<PackageInfo> getPackageInfo(String packageName) {
      return Optional.of(new PackageInfo("/data/app/base.apk", "/data/app/lib", "1"));
    }

    @Override
    public String getApkManifestDigest(String packagePath) throws Exception {
      throw new AdbCommandFailedException("unzip: not found");
    }

    @Override
    public String getContentHash(String path) {
      return hashes.getOrDefault(path, installedApkHash);
    }

    @Override
    public ImmutableSortedSet<Path> listDirRecursive(Path dirPath) {
      return ImmutableSortedSet.copyOf(files);
    }

    @Override
    public void rmStaleFiles(String packageName) {}

    @Override
    public void mkDirP(String dirpath) {}
  }

  private static class NoopPrinter implements AndroidInstallPrinter {
    @Override
    public void printMessage(String message) {}

    @Override
    public void printSuccess(String message) {}

    @Override
    public void printWarning(String message) {}

    @Override
    public void printError(String message) {}
  }
}
