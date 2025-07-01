/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.testrunner;

import com.android.ddmlib.FileListingService;
import com.android.ddmlib.IDevice;
import com.android.ddmlib.IShellEnabledDevice;
import com.android.ddmlib.IShellOutputReceiver;
import com.android.ddmlib.testrunner.InstrumentationResultParser;
import com.android.ddmlib.testrunner.RemoteAndroidTestRunner;
import com.facebook.buck.android.TestDevice;
import com.facebook.buck.testrunner.reportlayer.LogExtractorReportLayer;
import com.facebook.buck.testrunner.reportlayer.TombstonesReportLayer;
import com.facebook.buck.testrunner.reportlayer.VideoRecordingReportLayer;
import com.facebook.buck.testutil.TemporaryPaths;
import com.google.common.collect.ImmutableSet;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.net.URI;
import java.nio.charset.Charset;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;

/** Tests {@link InstrumentationTestRunner} */
public class InstrumentationTestRunnerTest {

  @Rule public final TemporaryPaths tmp = new TemporaryPaths();

  /** Just verifies the reflection is legit */
  @Test
  public void testSetTrimLinesHappyPath() throws Throwable {
    IShellEnabledDevice shellEnabledDevice = new TestDevice();
    RemoteAndroidTestRunner runner =
        new RemoteAndroidTestRunner("foobar", "blah", shellEnabledDevice);

    Field field = RemoteAndroidTestRunner.class.getDeclaredField("mParser");
    field.setAccessible(true);
    field.set(runner, new InstrumentationResultParser("fooBar", new ArrayList<>()));

    InstrumentationTestRunner.setTrimLine(runner, true);
  }

  @Test
  public void testSyncExopackageDir() throws Exception {
    final Set<String> pushedFilePaths = new HashSet<>();
    IDevice device =
        new TestDevice() {
          @Override
          public void pushFile(String local, String remote) {
            pushedFilePaths.add(remote);
          }
        };
    Path rootFolder = tmp.newFolder("dummy_exo_contents").getPath();
    // Set up the files to be written, including the metadata file which specifies the base
    // directory
    Files.write(
        rootFolder.resolve("metadata.txt"), "/data/local/tmp/exopackage/com.example".getBytes());
    Path contents = rootFolder.resolve(Paths.get("a", "b", "c"));
    Files.createDirectories(contents);
    Files.write(contents.resolve("foo"), "Hello World".getBytes());

    // Perform the sync
    InstrumentationTestRunner.syncExopackageDir(rootFolder, device);

    // Now verify the results
    Set<String> expectedPaths =
        ImmutableSet.of(
            "/data/local/tmp/exopackage/com.example/metadata.txt",
            "/data/local/tmp/exopackage/com.example/a/b/c/foo");
    Assert.assertEquals(expectedPaths, pushedFilePaths);
  }

  @Test
  public void runsTestAndClearsLogcat() throws Throwable {
    ArrayList<String> shellCommands = new ArrayList<>();
    IDevice device = createCommandCapturingTestDevice(shellCommands, Collections.emptyMap());
    InstrumentationTestRunner runner = createInstrumentationTestRunnerWithDevice(device);
    runner.run();
    List<String> expectedCommands =
        Arrays.asList(
            "logcat -G 16M",
            "logcat -c",
            "am instrument -w -r   com.example.test/com.example.test.TestRunner",
            "logcat -G 256K");
    Assert.assertEquals(expectedCommands, shellCommands);
  }

  @Test
  public void clearsPackageData() throws Throwable {
    ArrayList<String> shellCommands = new ArrayList<>();
    IDevice device = createCommandCapturingTestDevice(shellCommands, Collections.emptyMap());

    InstrumentationTestRunner runner =
        createInstrumentationTestRunnerWithDevice(device, "--clear-package-data");
    runner.run();
    List<String> expectedCommands =
        Arrays.asList(
            "pm clear com.example.test",
            "pm clear com.example",
            "logcat -G 16M",
            "logcat -c",
            "am instrument -w -r   com.example.test/com.example.test.TestRunner",
            "logcat -G 256K");
    Assert.assertEquals(expectedCommands, shellCommands);
  }

  @Test
  public void disablesAnimations() throws Throwable {
    ArrayList<String> shellCommands = new ArrayList<>();
    Map<String, String> mockShellResponses =
        new HashMap<String, String>() {
          {
            put("settings get global window_animation_scale", "1.0");
            put("settings get global transition_animation_scale", "0.9");
            put("settings get global animator_duration_scale", "0.8");
          }
        };
    IDevice device = createCommandCapturingTestDevice(shellCommands, mockShellResponses);

    InstrumentationTestRunner runner =
        createInstrumentationTestRunnerWithDevice(device, "--disable-animations");
    runner.run();
    Assert.assertEquals(13, shellCommands.size());

    // Verify that the animations are disabled
    Assert.assertTrue(shellCommands.contains("settings put global window_animation_scale 0.0"));
    Assert.assertTrue(shellCommands.contains("settings put global transition_animation_scale 0.0"));
    Assert.assertTrue(shellCommands.contains("settings put global animator_duration_scale 0.0"));

    // Verify that the animations are restored
    List<String> lastFourCommands =
        shellCommands.subList(shellCommands.size() - 4, shellCommands.size());
    Assert.assertTrue(lastFourCommands.contains("settings put global window_animation_scale 1.0"));
    Assert.assertTrue(
        lastFourCommands.contains("settings put global transition_animation_scale 0.9"));
    Assert.assertTrue(lastFourCommands.contains("settings put global animator_duration_scale 0.8"));
  }

  private void addOutput(IShellOutputReceiver receiver, String data) {
    byte[] bytes = data.getBytes(Charset.forName("UTF-8"));
    receiver.addOutput(bytes, 0, bytes.length);
  }

  private void mockLsCommandOutput(IShellOutputReceiver receiver, String[] files, String lsCommand)
      throws UnsupportedOperationException {
    HashMap<String, Set<String>> directoryToFiles = new HashMap<>();
    for (String file : files) {
      if (!file.startsWith("/")) {
        throw new UnsupportedOperationException("file on Android device should start with /");
      }
      String key = "/";
      for (String token : file.substring(1).split("/")) {
        if (!directoryToFiles.containsKey(key)) {
          directoryToFiles.put(key, new HashSet<>());
        }
        directoryToFiles.get(key).add(token);
        key += token;
        key += "/";
      }
    }

    System.out.println("ls command:" + lsCommand);
    String[] parts = lsCommand.split(" ");
    String baseDir = parts[parts.length - 1];

    for (String entry : directoryToFiles.get(baseDir)) {
      addOutput(receiver, String.format("drwxr-xr-x root root 0 2024-06-12 12:01 %s\n", entry));
    }
  }

  @Test
  public void extractLogWithRegex() throws Throwable {
    // set up TRA
    Map<String, String> env = new HashMap<>();
    Path tra = Files.createTempDirectory("ait-tra-");
    Path tra_annot = Files.createTempDirectory("ait-tra-annot-");
    env.put("TEST_RESULT_ARTIFACTS_DIR", tra.toString());
    env.put("TEST_RESULT_ARTIFACT_ANNOTATIONS_DIR", tra_annot.toString());

    ArrayList<String> expectedShellCommands = new ArrayList<>();
    expectedShellCommands.add("logcat -c");
    expectedShellCommands.add("am instrument -w -r   com.example.test/com.example.test.TestRunner");
    expectedShellCommands.add("logcat -d -b \"crash\"");
    expectedShellCommands.add("logcat -d -b \"system\"");
    expectedShellCommands.add("logcat -d -b \"main\"");

    IDevice device = createCommandCapturingTestDevice(expectedShellCommands, env);

    InstrumentationTestRunner runner =
        createInstrumentationTestRunnerWithDevice(
            device, new HashMap<>(), env, "--log-extractor", "Breadcrumbs=TEST_BREADCRUMBS:");
    runner.run();
    Path breadcrumbsTra = tra.resolve("Breadcrumbs");
    Path breadcrumbsTraAnnotation = tra_annot.resolve("Breadcrumbs.annotation");
    Assert.assertTrue(Files.exists(breadcrumbsTra));
    Assert.assertTrue(Files.exists(breadcrumbsTraAnnotation));

    Assert.assertEquals(
        "{\"type\": {\"generic_text_log\": {}}, \"description\": \"Breadcrumbs\"}",
        Files.readString(breadcrumbsTraAnnotation));
  }

  @Test
  public void collectVideoRecordingToTRA() throws Throwable {
    // set up TRA
    Map<String, String> env = new HashMap<>();
    Path tra = Files.createTempDirectory("ait-tra-");
    Path tra_annot = Files.createTempDirectory("ait-tra-annot-");
    env.put("TEST_RESULT_ARTIFACTS_DIR", tra.toString());
    env.put("TEST_RESULT_ARTIFACT_ANNOTATIONS_DIR", tra_annot.toString());

    ArrayList<String> expectedShellCommands = new ArrayList<>();
    expectedShellCommands.add("logcat -G 16M");
    expectedShellCommands.add("logcat -c");
    expectedShellCommands.add("am instrument -w -r   com.example.test/com.example.test.TestRunner");
    expectedShellCommands.add("logcat -d -b \"crash\"");
    expectedShellCommands.add("logcat -d -b \"system\"");
    expectedShellCommands.add("logcat -d -b \"main\"");
    expectedShellCommands.add("logcat -G 256K");

    IDevice device =
        new TestDevice() {
          @Override
          public void executeShellCommand(String command, IShellOutputReceiver receiver) {
            System.out.println("executeShellCommand:" + command);
            if (expectedShellCommands.contains(command)) {
              // this is an expected command, do nothing
              return;
            } else if (command.startsWith("ls -l")) {
              mockLsCommandOutput(
                  receiver,
                  new String[] {
                    "/storage/emulated/0/Android/data/com.example.test/files/test-video-record.sh",
                    "/storage/emulated/0/Android/data/com.example.test/files/test-video-record.mp4"
                  },
                  command);
            } else {
              System.out.println("unexpected command:" + command);
              throw new UnsupportedOperationException(
                  String.format("This command is not implemented in the mock: %s", command));
            }
          }

          @Override
          public FileListingService getFileListingService() {
            return new FileListingService(this);
          }
        };

    Map<Path, byte[]> files = new HashMap<>();
    files.put(
        Paths.get("/storage/emulated/0/Android/data/com.example.test/files/test-video-record.mp4"),
        "test-video-record.mp4".getBytes());
    InstrumentationTestRunner runner =
        createInstrumentationTestRunnerWithDevice(device, files, env, "--record-video");
    runner.run();
    Path video_file = tra.resolve("test-video-record.mp4");
    Path video_file_annotation = tra_annot.resolve("test-video-record.mp4.annotation");
    Assert.assertTrue(Files.exists(video_file_annotation));
    Assert.assertTrue(Files.exists(video_file));

    Assert.assertEquals(
        "{\"type\": {\"video_recording_test_artifact\": {}}, \"description\": \"video recording\"}",
        Files.readString(video_file_annotation));
  }

  @Test
  public void reportsTombstonesToTRA() throws Throwable {
    // set up TRA
    Map<String, String> env = new HashMap<>();
    Path tra = Files.createTempDirectory("ait-tra-");
    Path tra_annot = Files.createTempDirectory("ait-tra-annot-");
    env.put("TEST_RESULT_ARTIFACTS_DIR", tra.toString());
    env.put("TEST_RESULT_ARTIFACT_ANNOTATIONS_DIR", tra_annot.toString());

    ArrayList<String> expectedShellCommands = new ArrayList<>();
    expectedShellCommands.add("logcat -G 16M");
    expectedShellCommands.add("logcat -c");
    expectedShellCommands.add("am instrument -w -r   com.example.test/com.example.test.TestRunner");
    expectedShellCommands.add("logcat -d -b \"crash\"");
    expectedShellCommands.add("logcat -d -b \"system\"");
    expectedShellCommands.add("logcat -d -b \"main\"");
    expectedShellCommands.add("logcat -G 256K");
    IDevice device =
        new TestDevice() {
          @Override
          public void executeShellCommand(String command, IShellOutputReceiver receiver) {
            if (expectedShellCommands.contains(command)) {
              // this is an expected command, do nothing
              return;
            } else if (command.startsWith("ls -l")) {
              mockLsCommandOutput(receiver, new String[] {"/data/tombstones/tombstone"}, command);
            } else if (command.equals("test -d /data/tombstones/ && echo exists")) {
              byte[] response = "exists\n".getBytes();
              receiver.addOutput(response, 0, response.length);
            } else {
              throw new UnsupportedOperationException(
                  String.format("This command is not implemented in the mock: %s", command));
            }
          }

          @Override
          public FileListingService getFileListingService() {
            return new FileListingService(this);
          }
        };

    Map<Path, byte[]> files = new HashMap<>();
    files.put(Paths.get("/data/tombstones/tombstone"), "tombstone".getBytes());
    InstrumentationTestRunner runner =
        createInstrumentationTestRunnerWithDevice(device, files, env, "--collect-tombstones");
    runner.run();
    Path tombstone_zip = tra.resolve("tombstones.zip");
    Path tombstone_annotation = tra_annot.resolve("tombstones.zip.annotation");
    Assert.assertTrue(Files.exists(tombstone_zip));
    Assert.assertTrue(Files.exists(tombstone_annotation));

    Assert.assertEquals(
        "{\"type\": {\"generic_blob\": {}}, \"description\": \"Zip file with all tombstones\"}",
        Files.readString(tombstone_annotation));

    URI uri = URI.create("jar:" + tombstone_zip.toUri());
    HashMap<String, String> zipenv = new HashMap<>();
    zipenv.put("create", "false");
    try (FileSystem zipfs = FileSystems.newFileSystem(uri, zipenv)) {
      Path tombstone = zipfs.getPath("tombstone");
      Assert.assertEquals("tombstone", Files.readString(tombstone));
    }
  }

  @Test
  public void reportsTombstonesDirectoryToTRA() throws Throwable {
    // set up TRA
    Map<String, String> env = new HashMap<>();
    Path tra = Files.createTempDirectory("ait-tra-");
    Path tra_annot = Files.createTempDirectory("ait-tra-annot-");
    env.put("TEST_RESULT_ARTIFACTS_DIR", tra.toString());
    env.put("TEST_RESULT_ARTIFACT_ANNOTATIONS_DIR", tra_annot.toString());

    ArrayList<String> expectedShellCommands = new ArrayList<>();
    expectedShellCommands.add("logcat -G 16M");
    expectedShellCommands.add("logcat -c");
    expectedShellCommands.add("am instrument -w -r   com.example.test/com.example.test.TestRunner");
    expectedShellCommands.add("logcat -d -b \"crash\"");
    expectedShellCommands.add("logcat -d -b \"system\"");
    expectedShellCommands.add("logcat -d -b \"main\"");
    expectedShellCommands.add("logcat -G 256K");
    IDevice device =
        new TestDevice() {
          @Override
          public void executeShellCommand(String command, IShellOutputReceiver receiver) {
            if (expectedShellCommands.contains(command)) {
              // this is an expected command, do nothing
              return;
            } else if (command.startsWith("ls -l")) {
              mockLsCommandOutput(
                  receiver, new String[] {"/data/tombstones/subdir/tombstone"}, command);
            } else if (command.equals("test -d /data/tombstones/ && echo exists")) {
              byte[] response = "exists\n".getBytes();
              receiver.addOutput(response, 0, response.length);
            } else {
              throw new UnsupportedOperationException(
                  String.format("This command is not implemented in the mock: %s", command));
            }
          }

          @Override
          public FileListingService getFileListingService() {
            return new FileListingService(this);
          }
        };

    Map<Path, byte[]> files = new HashMap<>();
    files.put(Paths.get("/data/tombstones/subdir/tombstone"), "tombstone".getBytes());
    InstrumentationTestRunner runner =
        createInstrumentationTestRunnerWithDevice(device, files, env, "--collect-tombstones");
    runner.run();
    Path tombstone_zip = tra.resolve("tombstones.zip");
    Path tombstone_annotation = tra_annot.resolve("tombstones.zip.annotation");
    Assert.assertTrue(Files.exists(tombstone_zip));
    Assert.assertTrue(Files.exists(tombstone_annotation));

    Assert.assertEquals(
        "{\"type\": {\"generic_blob\": {}}, \"description\": \"Zip file with all tombstones\"}",
        Files.readString(tombstone_annotation));

    URI uri = URI.create("jar:" + tombstone_zip.toUri());
    HashMap<String, String> zipenv = new HashMap<>();
    zipenv.put("create", "false");
    try (FileSystem zipfs = FileSystems.newFileSystem(uri, zipenv)) {
      Path tombstone = zipfs.getPath("tombstone");
      Assert.assertEquals("tombstone", Files.readString(tombstone));
    }
  }

  @Test
  public void addsInstrumentationArgsFromPrefixedEnv() throws Throwable {
    Map<String, String> env = new HashMap<>();
    env.put("AIT_TEST_ONE", "foo");
    env.put("AIT_TEST_TWO", "bar");
    env.put("OTHER_RANDOM_ENV_VAR", "not");
    String expectedCommandPrefix = "am instrument";
    ArrayList<String> actualCommands = new ArrayList<>();
    IDevice device =
        new TestDevice() {
          @Override
          public void executeShellCommand(String command, IShellOutputReceiver receiver) {
            if (command.startsWith(expectedCommandPrefix)) {
              actualCommands.add(command);
            }
            // Ignore all other commands
            return;
          }
        };
    InstrumentationTestRunner runner =
        createInstrumentationTestRunnerWithDevice(device, new HashMap<>(), env);

    runner.run();

    Assert.assertTrue(actualCommands.size() == 1);
    String actualCommand = actualCommands.get(0);
    Assert.assertTrue(actualCommand.contains("-e TEST_ONE foo"));
    Assert.assertTrue(actualCommand.contains("-e TEST_TWO bar"));
    Assert.assertFalse(actualCommand.contains("RANDOM_ENV_VAR"));
  }

  private IDevice createCommandCapturingTestDevice(
      List<String> capturedShellCommands, Map<String, String> mockResponses) {
    return new TestDevice() {
      @Override
      public void executeShellCommand(String command, IShellOutputReceiver receiver) {
        capturedShellCommands.add(command);
        if (mockResponses.containsKey(command)) {
          byte[] response = mockResponses.get(command).getBytes();
          receiver.addOutput(response, 0, response.length);
        }
      }

      @Override
      public void executeShellCommand(
          String command,
          IShellOutputReceiver receiver,
          long maxTimeToOutputResponse,
          long l,
          TimeUnit maxTimeUnits) {
        capturedShellCommands.add(command);
      }

      @Override
      public FileListingService getFileListingService() {
        return new FileListingService(this);
      }
    };
  }

  private InstrumentationTestRunner createInstrumentationTestRunnerWithDevice(
      IDevice device, String... extraArgs) throws Throwable {
    return createInstrumentationTestRunnerWithDevice(
        device, new HashMap<>(), new HashMap<>(), extraArgs);
  }

  private InstrumentationTestRunner createInstrumentationTestRunnerWithDevice(
      IDevice device, Map<Path, byte[]> filesOnDevice, Map<String, String> env, String... extraArgs)
      throws Throwable {

    String[] args = {
      "--target-package-name",
      "com.example",
      "--test-package-name",
      "com.example.test",
      "--test-runner",
      "com.example.test.TestRunner",
      "--adb-executable-path",
      "required_but_not_used",
      "--output",
      "/dev/null",
      "--auto-run-on-connected-device"
    };

    String[] allArgs = new String[args.length + extraArgs.length];
    System.arraycopy(args, 0, allArgs, 0, args.length);
    System.arraycopy(extraArgs, 0, allArgs, args.length, extraArgs.length);

    InstrumentationTestRunner.ArgsParser argsParser = new InstrumentationTestRunner.ArgsParser();
    DeviceRunner.DeviceArgs deviceArgs = DeviceRunner.getDeviceArgs(args);
    argsParser.fromArgs(allArgs);
    InstrumentationTestRunner runner =
        new InstrumentationTestRunner(
            deviceArgs,
            argsParser.packageName,
            argsParser.targetPackageName,
            argsParser.testRunner,
            argsParser.outputDirectory,
            argsParser.instrumentationApkPath,
            argsParser.apkUnderTestPath,
            argsParser.exopackageLocalPath,
            argsParser.apkUnderTestExopackageLocalPath,
            argsParser.attemptUninstallApkUnderTest,
            argsParser.attemptUninstallInstrumentationApk,
            argsParser.debug,
            argsParser.codeCoverage,
            argsParser.codeCoverageOutputFile,
            argsParser.isSelfInstrumenting,
            argsParser.extraInstrumentationArguments,
            argsParser.extraInstrumentationTestListener,
            argsParser.extraFilesToPull,
            argsParser.extraDirsToPull,
            argsParser.clearPackageData,
            argsParser.disableAnimations,
            argsParser.preTestSetupScript,
            argsParser.extraApksToInstall) {

          @Override
          public Process exec(String command) throws IOException {
            return new Process() {
              @Override
              public OutputStream getOutputStream() {
                return System.out;
              }

              @Override
              public InputStream getInputStream() {
                return null;
              }

              @Override
              public InputStream getErrorStream() {
                return null;
              }

              @Override
              public int waitFor() throws InterruptedException {
                return 0;
              }

              @Override
              public int exitValue() {
                return 0;
              }

              @Override
              public void destroy() {}
            };
          }

          @Override
          protected IDevice getAndroidDevice(
              boolean autoRunOnConnectedDevice, String deviceSerial) {
            return device;
          }

          @Override
          protected void pullWithSyncService(
              IDevice device, FileListingService.FileEntry[] filesToPull, String destinationDir)
              throws Exception {
            for (FileListingService.FileEntry file : filesToPull) {
              Path path = Paths.get(file.getFullPath());
              for (Map.Entry<Path, byte[]> entry : filesOnDevice.entrySet()) {
                if (entry.getKey().equals(path)) {
                  Path target = Paths.get(destinationDir).resolve(entry.getKey().getFileName());
                  Files.write(target, entry.getValue());
                } else if (entry.getKey().startsWith(path)) {
                  Path target = Paths.get(destinationDir).resolve(path.relativize(entry.getKey()));
                  Files.write(target, entry.getValue());
                }
              }
            }
          }

          @Override
          public void pullFileWithSyncService(IDevice device, String remote, String local)
              throws Exception {
            for (Map.Entry<Path, byte[]> entry : filesOnDevice.entrySet()) {
              Path remote_path = Paths.get(remote);
              System.out.printf("remote: %s -> local: %s", remote, local);
              System.out.printf("entry: %s", entry.getKey().toString());
              if (entry.getKey().equals(remote_path)) {
                Path target = Paths.get(local);
                Files.write(target, entry.getValue());
              }
            }
          }

          @Override
          public void pushFileWithSyncService(IDevice device, String local, String remote)
              throws Exception {
            System.out.printf("mock: push file, local: %s -> remote: %s", local, remote);
          }

          @Override
          protected String getenv(String envVar) {
            return env.get(envVar);
          }

          @Override
          protected Map<String, String> getenv() {
            return env;
          }
        };
    if (argsParser.recordVideo) {
      runner.addReportLayer(new VideoRecordingReportLayer(runner));
    }
    if (argsParser.collectTombstones) {
      runner.addReportLayer(new TombstonesReportLayer(runner));
    }
    if (!argsParser.logExtractors.isEmpty()) {
      runner.addReportLayer(new LogExtractorReportLayer(runner, argsParser.logExtractors));
    }
    return runner;
  }
}
