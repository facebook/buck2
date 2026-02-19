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

import com.android.ddmlib.IDevice;
import com.android.ddmlib.MultiLineReceiver;
import com.android.ddmlib.testrunner.ITestRunListener;
import com.android.ddmlib.testrunner.RemoteAndroidTestRunner;
import com.android.ddmlib.testrunner.TestIdentifier;
import com.facebook.buck.android.exopackage.AdbUtils;
import com.facebook.buck.android.exopackage.AndroidDevice;
import com.facebook.buck.android.exopackage.AndroidDeviceImpl;
import com.facebook.buck.testresultsoutput.TestResultsOutputSender;
import com.facebook.buck.testrunner.reportlayer.LogExtractorReportLayer;
import com.facebook.buck.testrunner.reportlayer.PerfettoReportLayer;
import com.facebook.buck.testrunner.reportlayer.ReportLayer;
import com.facebook.buck.testrunner.reportlayer.TombstonesReportLayer;
import com.facebook.buck.testrunner.reportlayer.VideoRecordingReportLayer;
import com.facebook.buck.util.escaper.BashEscaper;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.nio.charset.Charset;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Stream;

/** Logcat buffers that can be collected from the device */
enum LogcatBuffer {
  MAIN("main"),
  SYSTEM("system"),
  RADIO("radio"),
  EVENTS("events"),
  CRASH("crash"),
  DEFAULT("default"),
  ALL("all");

  private String cliArgument;

  LogcatBuffer(String cliArgument) {
    this.cliArgument = cliArgument;
  }

  public String getCliArgument() {
    return this.cliArgument;
  }
}

public class InstrumentationTestRunner extends DeviceRunner {

  private static final String TEST_RESULT_ARTIFACTS_ENV = "TEST_RESULT_ARTIFACTS_DIR";
  private static final String TEST_RESULT_DIMENSIONS_ENV = "TEST_RESULT_DIMENSIONS_FILE";
  private static final String TEST_RESULT_APP_SCOPED_ARTIFACTS_ENV =
      "TEST_RESULT_APP_SCOPED_ARTIFACTS_DIR";
  private static final String TEST_RESULT_ARTIFACTS_ANNOTATIONS_ENV =
      "TEST_RESULT_ARTIFACT_ANNOTATIONS_DIR";
  private static final String TEST_RESULT_APP_SCOPED_ARTIFACTS_ANNOTATIONS_ENV =
      "TEST_RESULT_APP_SCOPED_ARTIFACT_ANNOTATIONS_DIR";
  private static final String FORWARDABLE_ENV_PREFIX = "AIT_";

  /** Env var to enable per-test timeout enforcement. */
  static final String PER_TEST_TIMEOUT_ENABLED_ENV = "ANDROID_PER_TEST_TIMEOUT_ENABLED";

  /** Env var to set the timeout multiplier for long-running tests. */
  static final String PER_TEST_TIMEOUT_MULTIPLIER_ENV = "ANDROID_PER_TEST_TIMEOUT_MULTIPLIER";

  private static final String INSTRUMENTATION_TEST_DEFAULT_ARTIFACTS_DIR_TEMPLATE =
      "/sdcard/test_result/%s/%s/";
  private static final String INSTRUMENTATION_TEST_DEFAULT_ARTIFACTS_FILE_TEMPLATE =
      "/sdcard/test_result/%s_%s";
  private static final String INSTRUMENTATION_TEST_APP_SCOPED_API_30_PLUS_ARTIFACTS_DIR_TEMPLATE =
      "/sdcard/Android/data/%s/files/Download/test_result/%s/";
  private static final String INSTRUMENTATION_TEST_APP_SCOPED_API_34_PLUS_ARTIFACTS_DIR_TEMPLATE =
      "/storage/emulated/0/Android/media/%s/test_result/%s/";
  private static final String PRE_TEST_SETUP_SCRIPT = "PRE_TEST_SETUP_SCRIPT";
  private static final String APEXES_TO_INSTALL = "APEXES_TO_INSTALL";

  private final String packageName;
  private final String targetPackageName;
  private final String testRunner;
  private final File outputDirectory;
  private final String exopackageLocalPath;
  private final boolean attemptUninstallApkUnderTest;
  private final boolean attemptUninstallInstrumentationApk;
  private final Map<String, String> extraInstrumentationArguments;
  private final String extraInstrumentationTestListener;
  private final Map<String, String> extraFilesToPull;
  private final Map<String, String> extraDirsToPull;
  private final boolean debug;
  private final boolean codeCoverage;
  private final boolean isSelfInstrumenting;
  private final List<LogcatBuffer> collectedLogcatBuffers;
  private final boolean clearPackageData;
  private final boolean disableAnimations;
  @Nullable private final String instrumentationApkPath;
  @Nullable private final String apkUnderTestPath;
  @Nullable private final String codeCoverageOutputFile;
  @Nullable private final String apkUnderTestExopackageLocalPath;
  private final String preTestSetupScript;
  private final List<String> apexesToInstall;

  @Nullable private final Integer userId;

  /** Prefix for secondary user storage paths */
  private static final String SECONDARY_USER_STORAGE_PREFIX = "/data/media/";

  private static final String SDCARD_PATH = "/sdcard";
  private static final String STORAGE_EMULATED_PATH = "/storage/emulated/0";
  private static final String STORAGE_EMULATED_PREFIX = "/storage/emulated/";

  private final CrashAnalyzer crashAnalyzer = new CrashAnalyzer();
  private List<ReportLayer> reportLayers = new ArrayList<>();

  private IDevice device = null;
  protected final AndroidDevice androidDevice;
  protected final AdbUtils adbUtils;
  private volatile boolean testRunFailed = false;

  /**
   * Resolves a device path for the current user. For secondary users, paths like /sdcard and
   * /storage/emulated/0 are translated to /data/media/{userId}.
   *
   * <p>Also handles paths that already contain a user-specific storage path prefix (e.g.,
   * /storage/emulated/10) by translating them to the current user's storage path.
   *
   * @param path The original device path
   * @return The resolved path appropriate for the current user
   */
  private String resolvePathForUser(String path) {
    if (userId == null || userId == 0) {
      return path;
    }

    String userStoragePath = SECONDARY_USER_STORAGE_PREFIX + userId;

    if (path.startsWith(SDCARD_PATH)) {
      return userStoragePath + path.substring(SDCARD_PATH.length());
    }

    // Handle /storage/emulated/0 specifically (primary user path)
    if (path.startsWith(STORAGE_EMULATED_PATH)) {
      return userStoragePath + path.substring(STORAGE_EMULATED_PATH.length());
    }

    // Handle /storage/emulated/{other_user_id} paths (e.g., /storage/emulated/10)
    if (path.startsWith(STORAGE_EMULATED_PREFIX)) {
      // Find the end of the user ID portion
      int userIdEndIndex = path.indexOf('/', STORAGE_EMULATED_PREFIX.length());
      if (userIdEndIndex == -1) {
        // Path is just /storage/emulated/{userId} with no trailing content
        return userStoragePath;
      }
      // Extract the rest of the path after /storage/emulated/{userId}
      String remainingPath = path.substring(userIdEndIndex);
      return userStoragePath + remainingPath;
    }

    return path;
  }

  public InstrumentationTestRunner(
      DeviceArgs deviceArgs,
      String packageName,
      String targetPackageName,
      String testRunner,
      File outputDirectory,
      String instrumentationApkPath,
      String apkUnderTestPath,
      String exopackageLocalPath,
      String apkUnderTestExopackageLocalPath,
      boolean attemptUninstallApkUnderTest,
      boolean attemptUninstallInstrumentationApk,
      boolean debug,
      boolean codeCoverage,
      String codeCoverageOutputFile,
      boolean isSelfInstrumenting,
      Map<String, String> extraInstrumentationArguments,
      String extraInstrumentationTestListener,
      Map<String, String> extraFilesToPull,
      Map<String, String> extraDirsToPull,
      boolean clearPackageData,
      boolean disableAnimations,
      String preTestSetupScript,
      List<String> apexesToInstall,
      @Nullable Integer userId) {
    super(deviceArgs);
    this.packageName = packageName;
    this.targetPackageName = targetPackageName;
    this.testRunner = testRunner;
    this.outputDirectory = outputDirectory;
    this.instrumentationApkPath = instrumentationApkPath;
    this.apkUnderTestPath = apkUnderTestPath;
    this.exopackageLocalPath = exopackageLocalPath;
    this.apkUnderTestExopackageLocalPath = apkUnderTestExopackageLocalPath;
    this.attemptUninstallApkUnderTest = attemptUninstallApkUnderTest;
    this.attemptUninstallInstrumentationApk = attemptUninstallInstrumentationApk;
    this.codeCoverageOutputFile = codeCoverageOutputFile;
    this.extraInstrumentationArguments = extraInstrumentationArguments;
    this.extraInstrumentationTestListener = extraInstrumentationTestListener;
    this.extraFilesToPull = extraFilesToPull;
    this.extraDirsToPull = extraDirsToPull;
    this.isSelfInstrumenting = isSelfInstrumenting;
    this.debug = debug;
    this.codeCoverage = codeCoverage;
    this.collectedLogcatBuffers =
        Arrays.asList(LogcatBuffer.CRASH, LogcatBuffer.SYSTEM, LogcatBuffer.MAIN);
    this.clearPackageData = clearPackageData;
    this.disableAnimations = disableAnimations;
    this.preTestSetupScript = preTestSetupScript;
    this.apexesToInstall = apexesToInstall;
    this.userId = userId;
    this.adbUtils = new AdbUtils(getAdbPath(), 0);
    this.androidDevice = initializeAndroidDevice();
  }

  protected static class ArgsParser {
    File outputDirectory = null;
    DeviceArgs deviceArgs = null;
    String apkUnderTestPath = null;
    String packageName = null;
    String targetPackageName = null;
    String testRunner = null;
    String instrumentationApkPath = null;
    String codeCoverageOutputFile = null;
    String exopackageLocalPath = null;
    String apkUnderTestExopackageLocalPath = null;
    String extraInstrumentationTestListener = null;
    boolean attemptUninstallApkUnderTest = false;
    boolean attemptUninstallInstrumentationApk = false;
    boolean debug = false;
    boolean codeCoverage = false;
    boolean isSelfInstrumenting = false;
    boolean clearPackageData = false;
    boolean disableAnimations = false;
    boolean collectTombstones = false;
    boolean recordVideo = false;
    boolean collectPerfetto = false;
    Map<String, String> extraInstrumentationArguments = new HashMap<String, String>();
    Map<String, String> extraFilesToPull = new HashMap<String, String>();
    Map<String, String> extraDirsToPull = new HashMap<String, String>();
    Map<String, String> logExtractors = new HashMap<String, String>();
    String preTestSetupScript = null;
    List<String> extraApksToInstall = new ArrayList<>();
    @Nullable Integer userId = null;

    @SuppressWarnings("PMD.BlacklistedSystemGetenv")
    void fromArgs(String... args) throws IOException {

      for (int i = 0; i < args.length; i++) {
        switch (args[i]) {
          case "--test-package-name":
            packageName = getArgValue(args[++i]);
            break;
          case "--target-package-name":
            targetPackageName = getArgValue(args[++i]);
            break;
          case "--test-runner":
            testRunner = getArgValue(args[++i]);
            break;
          case "--output":
            outputDirectory = new File(args[++i]);
            if (!outputDirectory.exists() && !outputDirectory.mkdirs()) {
              System.err.printf(
                  "The output directory did not exist and failed to create it: %s\n",
                  outputDirectory);
              System.exit(1);
            }
            break;
          case "--extra-instrumentation-test-listener":
            extraInstrumentationTestListener = args[++i];
            break;
          case "--apk-under-test-path":
            apkUnderTestPath = args[++i];
            break;
          case "--instrumentation-apk-path":
            instrumentationApkPath = args[++i];
            break;
          case "--exopackage-local-dir":
            exopackageLocalPath = args[++i];
            break;
          case "--apk-under-test-exopackage-local-dir":
            apkUnderTestExopackageLocalPath = args[++i];
            break;
          case "--attempt-uninstall":
            attemptUninstallApkUnderTest = true;
            attemptUninstallInstrumentationApk = true;
            break;
          case "--attempt-uninstall-apk-under-test":
            attemptUninstallApkUnderTest = true;
            break;
          case "--attempt-uninstall-instrumentation-apk":
            attemptUninstallInstrumentationApk = true;
            break;
          case "--debug":
            debug = true;
            break;
          case "--code-coverage":
            codeCoverage = true;
            break;
          case "--code-coverage-output-file":
            codeCoverageOutputFile = args[++i];
            break;
          case "--is-self-instrumenting":
            isSelfInstrumenting = true;
            break;
          case "--extra-instrumentation-argument":
            String rawArg = args[++i];
            String[] extraArguments = rawArg.split("=", 2);
            if (extraArguments.length != 2) {
              System.err.printf("Not a valid extra arguments argument: %s\n", rawArg);
              System.exit(1);
            }
            extraInstrumentationArguments.put(extraArguments[0], extraArguments[1]);
            break;
          case "--extra-file-to-pull":
            // Format is --extra-file-to-pull /source/path/in/device.txt=/destination/path.txt
            // Pulls contents of source file into destination file.
            // Expects both parts of the argument to be files.
            // Expects destination directory to exist.
            {
              String raw = args[++i];
              String[] parts = raw.split("=", 2);
              if (parts.length != 2) {
                System.err.printf("Not a valid file to pull: %s\n", raw);
                System.exit(1);
              }
              extraFilesToPull.put(parts[0], parts[1]);
              break;
            }
          case "--extra-dir-to-pull":
            // Format is --extra-dir-to-pull /device/source=/directory/destination
            // Pulls all files from source into destination directory.
            // Expects both parts of the argument to be directories.
            // Creates destination directory if it doesn't exist.
            // Creates clean source directory before the test run.
            {
              String raw = args[++i];
              String[] parts = raw.split("=", 2);
              if (parts.length != 2) {
                System.err.printf("Not a valid dir to pull: %s\n", raw);
                System.exit(1);
              }
              extraDirsToPull.put(parts[0], parts[1]);
              break;
            }
          case "--clear-package-data":
            clearPackageData = true;
            break;
          case "--disable-animations":
            disableAnimations = true;
            break;
          case TombstonesReportLayer.ARG:
            collectTombstones = true;
            break;
          case VideoRecordingReportLayer.ARG:
            recordVideo = true;
            break;
          case PerfettoReportLayer.ARG:
            collectPerfetto = true;
            break;
          case LogExtractorReportLayer.ARG:
            String logExtractorArg = args[++i];
            String[] logExtractorArgSplit = logExtractorArg.split("=", 2);
            if (logExtractorArgSplit.length != 2) {
              System.err.printf("Not a valid log extractor argument: %s\n", logExtractorArg);
              System.exit(1);
            }
            logExtractors.put(logExtractorArgSplit[0], logExtractorArgSplit[1]);
            break;
          case "--user":
            if (i + 1 >= args.length) {
              System.err.println("--user requires a user ID argument");
              System.exit(1);
            }
            String userIdArg = args[++i];
            try {
              userId = Integer.parseInt(userIdArg);
              if (userId < 0) {
                System.err.printf("Invalid user ID: %s\n", userIdArg);
                System.exit(1);
              }
            } catch (NumberFormatException e) {
              System.err.printf("Invalid user ID: %s\n", userIdArg);
              System.exit(1);
            }
            break;
        }
      }

      if (packageName == null) {
        System.err.println("Must pass --test-package-name argument.");
        System.exit(1);
      }

      if (targetPackageName == null) {
        System.err.println("Must pass --target-package-name argument.");
        System.exit(1);
      }

      if (testRunner == null) {
        System.err.println("Must pass --test-runner argument.");
        System.exit(1);
      }

      if (outputDirectory == null) {
        System.err.println("Must pass --output argument.");
        System.exit(1);
      }

      // Process env-based setup - using package name to uniquify the folders
      // and avoid concurrency problems

      String testArtifactsPath = System.getenv(TEST_RESULT_ARTIFACTS_ENV);
      if (testArtifactsPath != null) {
        String devicePath =
            String.format(
                INSTRUMENTATION_TEST_DEFAULT_ARTIFACTS_DIR_TEMPLATE, "artifacts", packageName);
        extraDirsToPull.put(devicePath, testArtifactsPath);
        extraInstrumentationArguments.put(TEST_RESULT_ARTIFACTS_ENV, devicePath);
      }

      String testArtifactAnnotationsPath = System.getenv(TEST_RESULT_ARTIFACTS_ANNOTATIONS_ENV);
      if (testArtifactAnnotationsPath != null) {
        String devicePath =
            String.format(
                INSTRUMENTATION_TEST_DEFAULT_ARTIFACTS_DIR_TEMPLATE,
                "artifact_annotations",
                packageName);
        extraDirsToPull.put(devicePath, testArtifactAnnotationsPath);
        extraInstrumentationArguments.put(TEST_RESULT_ARTIFACTS_ANNOTATIONS_ENV, devicePath);
      }

      String preTestSetupEnv = System.getenv(PRE_TEST_SETUP_SCRIPT);
      if (preTestSetupEnv != null) {
        this.preTestSetupScript = preTestSetupEnv;
      }

      String extraApksToInstall = System.getenv(APEXES_TO_INSTALL);
      if (extraApksToInstall != null) {
        this.extraApksToInstall = Arrays.asList(extraApksToInstall.split(","));
      }

      deviceArgs = getDeviceArgs(args);
    }
  }

  public String getPackageName() {
    return packageName;
  }

  public IDevice getDevice() {
    return this.device;
  }

  public boolean hasTestRunFailed() {
    return this.testRunFailed;
  }

  public void addReportLayer(ReportLayer reportLayer) {
    reportLayers.add(reportLayer);
  }

  @SuppressWarnings("PMD.BlacklistedSystemGetenv")
  public static InstrumentationTestRunner fromArgs(String... args) throws IOException {
    ArgsParser argsParser = new ArgsParser();
    argsParser.fromArgs(args);
    InstrumentationTestRunner runner =
        new InstrumentationTestRunner(
            argsParser.deviceArgs,
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
            argsParser.extraApksToInstall,
            argsParser.userId);
    if (argsParser.recordVideo) {
      runner.addReportLayer(new VideoRecordingReportLayer(runner));
    }
    runner.addReportLayer(new TombstonesReportLayer(runner, argsParser.collectTombstones));
    if (argsParser.collectPerfetto) {
      runner.addReportLayer(new PerfettoReportLayer(runner));
    }
    if (!argsParser.logExtractors.isEmpty()) {
      runner.addReportLayer(new LogExtractorReportLayer(runner, argsParser.logExtractors));
    }
    return runner;
  }

  private static class AnimationScales {
    public final float windowAnimationScale;
    public final float transitionAnimationScale;
    public final float animatorDurationScale;

    public AnimationScales(
        float windowAnimationScale, float transitionAnimationScale, float animatorDurationScale) {
      this.windowAnimationScale = windowAnimationScale;
      this.transitionAnimationScale = transitionAnimationScale;
      this.animatorDurationScale = animatorDurationScale;
    }
  }

  protected void installPackage(String path) throws Throwable {
    // When running as secondary user, install for all users so the APK is available
    // to the secondary user context
    String userTarget = (this.userId != null && this.userId > 0) ? "all" : null;
    androidDevice.installApkOnDevice(new File(path), false, false, true, false, userTarget);
  }

  /**
   * Execute adb shell command using AndroidDevice and AdbUtils.
   *
   * @param command the shell command to execute
   * @return the output of the shell command
   * @throws Exception if command execution fails
   */
  protected String executeAdbShellCommand(String command) throws Exception {
    return adbUtils.executeAdbShellCommand(command, androidDevice.getSerialNumber(), false);
  }

  protected AndroidDevice initializeAndroidDevice() {
    String deviceSerial = deviceArgs.deviceSerial;

    // Validate device selection arguments
    if (deviceSerial == null && !deviceArgs.autoRunOnConnectedDevice) {
      throw new IllegalArgumentException(
          "Either deviceSerial must be provided or autoRunOnConnectedDevice must be enabled");
    }

    AndroidDevice device = null;

    // If both deviceSerial and autoRunOnConnectedDevice are specified, deviceSerial takes
    // precedence
    if (deviceSerial == null && deviceArgs.autoRunOnConnectedDevice) {
      List<AndroidDevice> devices = this.adbUtils.getDevices();
      if (!devices.isEmpty()) {
        // TODO: If more than one device is attached, we currently select the first one.
        // Consider warning the user or providing a way to specify which device to use.
        device = devices.get(0);
      }
    } else if (deviceSerial != null) {
      device = new AndroidDeviceImpl(deviceSerial, this.adbUtils);
    }

    if (device == null) {
      throw new RuntimeException("Failed to initialize AndroidDevice");
    }

    return device;
  }

  @SuppressWarnings({"PMD.BlacklistedSystemGetenv", "PMD.BlacklistedDefaultProcessMethod"})
  public void run() throws Throwable {
    IDevice device = getAndroidDevice(deviceArgs.autoRunOnConnectedDevice, deviceArgs.deviceSerial);
    this.device = device;

    if (this.instrumentationApkPath != null) {
      if (this.apkUnderTestPath != null) {
        // Install both APKs in parallel to improve performance
        ExecutorService executor = Executors.newFixedThreadPool(2);
        try {
          Future<Void> instrumentationFuture =
              executor.submit(
                  () -> {
                    try {
                      installPackage(this.instrumentationApkPath);
                      return null;
                    } catch (Throwable t) {
                      throw new RuntimeException("Failed to install instrumentation APK", t);
                    }
                  });

          Future<Void> apkUnderTestFuture =
              executor.submit(
                  () -> {
                    try {
                      installPackage(this.apkUnderTestPath);
                      return null;
                    } catch (Throwable t) {
                      throw new RuntimeException("Failed to install APK under test", t);
                    }
                  });

          // Wait for both installations to complete and handle any exceptions
          try {
            instrumentationFuture.get();
            apkUnderTestFuture.get();
          } catch (ExecutionException e) {
            throw e.getCause();
          }
        } finally {
          executor.shutdown();
          executor.awaitTermination(60, TimeUnit.SECONDS);
        }
      } else {
        // Single APK installation (no APK under test)
        installPackage(this.instrumentationApkPath);
      }
    }

    if (this.exopackageLocalPath != null) {
      Path localBase = Paths.get(exopackageLocalPath);
      syncExopackageDir(localBase);
    }

    if (this.apkUnderTestExopackageLocalPath != null) {
      Path localBase = Paths.get(apkUnderTestExopackageLocalPath);
      syncExopackageDir(localBase);
    }

    String appScopedStorageDeviceArtifactsPath =
        getAppScopedStoragePath(packageName, targetPackageName, isSelfInstrumenting, "artifacts");
    if (appScopedStorageDeviceArtifactsPath != null) {
      String testArtifactsPath = getenv(TEST_RESULT_ARTIFACTS_ENV);
      if (testArtifactsPath != null) {
        extraDirsToPull.put(appScopedStorageDeviceArtifactsPath, testArtifactsPath);
        extraInstrumentationArguments.put(
            TEST_RESULT_APP_SCOPED_ARTIFACTS_ENV, appScopedStorageDeviceArtifactsPath);
      }
    }

    String testDimensionsPath = getenv(TEST_RESULT_DIMENSIONS_ENV);
    if (testDimensionsPath != null) {
      String appScopedStorageDeviceDimensionsPath =
          getAppScopedStoragePath(
              packageName, targetPackageName, isSelfInstrumenting, "dimensions", "dimensions.tsv");
      if (appScopedStorageDeviceDimensionsPath != null) {
        extraFilesToPull.put(appScopedStorageDeviceDimensionsPath, testDimensionsPath);
        extraInstrumentationArguments.put(
            TEST_RESULT_DIMENSIONS_ENV, appScopedStorageDeviceDimensionsPath);
      } else {
        String devicePath =
            String.format(
                INSTRUMENTATION_TEST_DEFAULT_ARTIFACTS_FILE_TEMPLATE,
                packageName,
                "dimensions.tsv");
        extraFilesToPull.put(devicePath, testDimensionsPath);
        extraInstrumentationArguments.put(TEST_RESULT_DIMENSIONS_ENV, devicePath);
      }
    }

    // Install the extra APEXes before the preTestSetupScript.
    // If installation requires reboot, such as if you are sideloading an APEX that doesn't support
    // rebootless updating per https://source.android.com/docs/core/ota/apex#installing-an-apex,
    // that can be added in preTestSetupScript, e.g.
    // arvr/projects/codec_avatar/prod/pre_test_setup_script_with_apex.sh.
    if (this.apexesToInstall != null && !this.apexesToInstall.isEmpty()) {
      System.err.println(String.format("Installing %d APEX(es)...", this.apexesToInstall.size()));

      // Prepare device for APEX installation once before the loop
      final boolean softRebootAvailable = androidDevice.prepareForApexInstallation();

      for (int i = 0; i < this.apexesToInstall.size(); i++) {
        final String apexPath = this.apexesToInstall.get(i);
        final boolean isLast = (i == this.apexesToInstall.size() - 1);

        System.err.println(
            String.format(
                "Installing APEX %d/%d: %s...", i + 1, this.apexesToInstall.size(), apexPath));
        androidDevice.installApexOnDevice(new File(apexPath), false, isLast, softRebootAvailable);
      }

      System.err.println(
          String.format("All %d APEX(es) installed successfully.", this.apexesToInstall.size()));
    }

    if (this.preTestSetupScript != null) {
      System.err.println("Running pre test setup: " + this.preTestSetupScript);
      try {
        Process process =
            new ProcessBuilder()
                .command(Arrays.asList(this.preTestSetupScript))
                .inheritIO()
                .start();
        System.err.println("Executing pre test setup process: " + this.preTestSetupScript);
        boolean completedInTime = process.waitFor(15, TimeUnit.MINUTES);
        if (completedInTime != true) {
          throw new RuntimeException("Pre-test setup script timed out");
        }
        int exitCode = process.exitValue();
        if (exitCode != 0) {
          throw new RuntimeException("Pre-test setup script failed with exit code: " + exitCode);
        }
      } catch (IOException | InterruptedException e) {
        throw new RuntimeException("An error occurred while running the pre-test setup script", e);
      }
      System.err.println("Pre test setup completed: " + this.preTestSetupScript);
    }

    String appScopedStorageDeviceAnnotationsPath =
        getAppScopedStoragePath(
            packageName, targetPackageName, isSelfInstrumenting, "artifact_annotations");
    if (appScopedStorageDeviceAnnotationsPath != null) {
      String testArtifactAnnotationsPath = System.getenv(TEST_RESULT_ARTIFACTS_ANNOTATIONS_ENV);
      if (testArtifactAnnotationsPath != null) {
        extraDirsToPull.put(appScopedStorageDeviceAnnotationsPath, testArtifactAnnotationsPath);
        extraInstrumentationArguments.put(
            TEST_RESULT_APP_SCOPED_ARTIFACTS_ANNOTATIONS_ENV,
            appScopedStorageDeviceAnnotationsPath);
      }
    }

    if (this.clearPackageData) {
      executeAdbShellCommand("pm clear " + this.packageName);
      executeAdbShellCommand("pm clear " + this.targetPackageName);
    }

    AnimationScales originalWindowAnimationScales = null;

    if (this.disableAnimations) {
      originalWindowAnimationScales = getAnimationScales();
      setAnimationScales(new AnimationScales(0f, 0f, 0f));
    }

    // Increase logcat buffer size to 16MB.
    executeAdbShellCommand("logcat -G 16M");

    // Clear logcat logs prior to test run
    executeAdbShellCommand("logcat -c");

    // Clean up output directories before the run
    for (final String devicePath : this.extraDirsToPull.keySet()) {
      String resolvedPath = resolvePathForUser(devicePath);
      String output = executeAdbShellCommand("rm -fr " + resolvedPath);

      if (directoryExists(resolvedPath)) {
        System.err.printf(
            "Failed to clean up directory %s due to error: %s\n", resolvedPath, output);
        System.exit(1);
      }

      output = executeAdbShellCommand("mkdir -p " + resolvedPath);
      if (!directoryExists(resolvedPath)) {
        System.err.printf("Failed to create directory %s due to error: %s\n", resolvedPath, output);
      }
    }

    for (ReportLayer layer : this.reportLayers) {
      layer.initialize();
    }

    try {
      RemoteAndroidTestRunner runner =
          new RemoteAndroidTestRunner(
              this.packageName,
              this.testRunner,
              getAndroidDevice(deviceArgs.autoRunOnConnectedDevice, deviceArgs.deviceSerial));

      for (Map.Entry<String, String> entry : this.extraInstrumentationArguments.entrySet()) {
        runner.addInstrumentationArg(
            entry.getKey(), BashEscaper.BASH_ESCAPER.apply(entry.getValue()));
      }

      // Set prefixed env vars as instrumentation args without the prefix.
      for (Map.Entry<String, String> entry : getenv().entrySet()) {
        if (entry.getKey().startsWith(FORWARDABLE_ENV_PREFIX)) {
          runner.addInstrumentationArg(
              entry.getKey().substring(FORWARDABLE_ENV_PREFIX.length()),
              BashEscaper.BASH_ESCAPER.apply(entry.getValue()));
        }
      }

      if (debug) {
        runner.setDebug(true);
      }

      String coverageTempDir = getenv("COVERAGE_COLLECTION_TMPDIR");
      boolean useJaCoCoCoverage = getenv("USE_JACOCO_COVERAGE") != null;
      if (codeCoverage || (useJaCoCoCoverage && coverageTempDir != null)) {
        runner.setCoverage(true);
      }
      ITestRunListener extraListener = null;
      if (this.extraInstrumentationTestListener != null) {
        Class<?> clazz = Class.forName(this.extraInstrumentationTestListener);
        Constructor<?>[] ctors = clazz.getDeclaredConstructors();
        ctors[0].setAccessible(true);
        extraListener = (ITestRunListener) ctors[0].newInstance(deviceArgs.deviceSerial, runner);
        if (extraListener == null) {
          System.err.printf("Failed to construct %s\n", this.extraInstrumentationTestListener);
          System.exit(1);
        }
      }

      BuckXmlTestRunListener buckXmlListener = new BuckXmlTestRunListener(androidDevice, adbUtils);
      ITestRunListener trimLineListener =
          new ITestRunListener() {
            /**
             * Before the actual run starts (and after the InstrumentationResultsParser is created),
             * we need to do some reflection magic to make RemoteAndroidTestRunner not trim
             * indentation from lines.
             */
            @Override
            public void testRunStarted(String runName, int testCount) {
              setTrimLine(runner, false);
            }

            @Override
            public void testRunEnded(long elapsedTime, Map<String, String> runMetrics) {}

            @Override
            public void testRunFailed(String errorMessage) {
              System.err.println("Test Run Failed: " + errorMessage);
              testRunFailed = true;
            }

            @Override
            public void testStarted(TestIdentifier test) {}

            @Override
            public void testFailed(TestIdentifier test, String trace) {}

            @Override
            public void testAssumptionFailure(TestIdentifier test, String trace) {}

            @Override
            public void testIgnored(TestIdentifier test) {}

            @Override
            public void testEnded(TestIdentifier test, Map<String, String> testMetrics) {}

            @Override
            public void testRunStopped(long elapsedTime) {}
          };

      buckXmlListener.setReportDir(this.outputDirectory);

      List<ITestRunListener> listeners = new ArrayList<ITestRunListener>();
      if (extraListener != null) {
        listeners.add(extraListener);
      }
      listeners.add(trimLineListener);
      listeners.add(buckXmlListener);

      // Add timeout enforcement listener if enabled
      if ("true".equals(System.getenv(PER_TEST_TIMEOUT_ENABLED_ENV))) {
        listeners.add(new InstrumentationTimeoutEnforcingRunListener(buckXmlListener));
      }

      Optional<TestResultsOutputSender> testResultsOutputSender =
          TestResultsOutputSender.fromDefaultEnvName();
      if (testResultsOutputSender.isPresent()) {
        InstrumentationTpxStandardOutputTestListener tpxListener =
            new InstrumentationTpxStandardOutputTestListener(
                testResultsOutputSender.get(), androidDevice, adbUtils);
        listeners.add(tpxListener);
      }

      if (this.userId != null) {
        runner.addInstrumentationArg("user", this.userId.toString());
      }
      runner.run(listeners);

      if (this.disableAnimations) {
        setAnimationScales(originalWindowAnimationScales);
      }

      if (this.codeCoverageOutputFile != null || (useJaCoCoCoverage && coverageTempDir != null)) {
        String destCovFileInHost = this.codeCoverageOutputFile;

        if (coverageTempDir != null) {
          (new File(coverageTempDir)).mkdirs();
          destCovFileInHost = coverageTempDir + "/target-package-coverage.ec";
        }

        String covFileInEmu = "/data/data/" + this.targetPackageName + "/files/coverage.ec";
        String covFileInSdcard = resolvePathForUser("/sdcard/coverage.ec");
        String cpOutput =
            executeAdbShellCommand("su root cp " + covFileInEmu + " " + covFileInSdcard);

        System.out.println(cpOutput);
        pullFile(covFileInSdcard, destCovFileInHost);

        String coverageParser = getenv("COVERAGE_PARSER");
        if (coverageParser != null) {

          ArrayList<String> cmd_args =
              new ArrayList<String>(Arrays.asList(coverageParser.split(" ")));

          System.out.println("coverge parser command: " + String.join(" ", cmd_args));

          ProcessBuilder coverageParserProcessBuilder = new ProcessBuilder().command(cmd_args);

          coverageParserProcessBuilder.inheritIO();

          Process coverageParserProcess = coverageParserProcessBuilder.start();
          coverageParserProcess.waitFor();
        }
      }
      for (Map.Entry<String, String> entry : this.extraFilesToPull.entrySet()) {
        String resolvedPath = resolvePathForUser(entry.getKey());
        pullFile(resolvedPath, entry.getValue());
      }
      for (Map.Entry<String, String> entry : this.extraDirsToPull.entrySet()) {
        String resolvedPath = resolvePathForUser(entry.getKey());
        pullDir(resolvedPath, entry.getValue());
      }

    } finally {
      this.collectAdbLogs();

      // Restore logcat buffer size to default.
      executeAdbShellCommand("logcat -G 256K");

      for (ReportLayer layer : this.reportLayers) {
        layer.report();
      }
      if (this.attemptUninstallInstrumentationApk) {
        try {
          androidDevice.uninstallPackage(this.packageName);
        } catch (Exception e) {
          System.err.printf("Failed to uninstall instrumentation package: %s\n", e);
        }
      }
      if (this.attemptUninstallApkUnderTest) {
        try {
          androidDevice.uninstallPackage(this.targetPackageName);
        } catch (Exception e) {
          System.err.printf("Failed to uninstall target package: %s\n", e);
        }
      }
    }
  }

  public Process exec(String command) throws IOException {
    return Runtime.getRuntime().exec(command);
  }

  @Nullable
  private String getDeviceLogcatOutputForBuffer(LogcatBuffer buffer) {
    String adbCommand = String.format("logcat -d -b \"%s\"", buffer.getCliArgument());

    try {
      return adbUtils.executeAdbShellCommand(adbCommand, androidDevice.getSerialNumber(), true);
    } catch (Exception e) {
      System.err.printf("Encountered an error attempting to pull logcat output %s\n", e);
      return null;
    }
  }

  /**
   * create TRA path for logcat buffer
   *
   * @param bufferName name of the buffer
   * @return path to the tra
   * @throws IOException when fail to create the path
   */
  public Path createPathForLogcatBuffer(String bufferName) throws IOException {
    return this.createTRALogcatLog(
        String.format("Logcat `%s` Buffer Output", bufferName),
        String.format("logcat_%s_buffer_logs", bufferName));
  }

  @SuppressWarnings("PMD.BlacklistedSystemGetenv")
  private void collectAdbLogs() {
    try {
      StringBuilder allLogOutput = new StringBuilder();

      for (LogcatBuffer buffer : this.collectedLogcatBuffers) {
        String bufferName = buffer.getCliArgument();
        Path traPath = this.createPathForLogcatBuffer(bufferName);
        if (traPath == null) {
          return;
        }
        String logOutput = getDeviceLogcatOutputForBuffer(buffer);
        if (logOutput == null) {
          continue;
        }

        // Accumulate all log output for crash analysis
        allLogOutput.append("=== ").append(bufferName.toUpperCase()).append(" BUFFER ===\n");
        allLogOutput.append(logOutput).append("\n");

        try (FileWriter logWriter = new FileWriter(traPath.toString())) {
          logWriter.write(logOutput);
        }
      }

      // Analyze all collected logcat output for crash information
      crashAnalyzer.analyzeCrashInformation(allLogOutput.toString());

    } catch (IOException e) {
      e.printStackTrace(System.err);
      System.err.printf("Failed to write logs from buffer failed with error: %s\n", e);
    }
  }

  /**
   * create TRA
   *
   * @param type TRA type
   * @param description description of to be created TRA
   * @param name name of the TRA
   * @return Path of the created TRA, null if the creation is not successful
   * @throws IOException the exception may throw from file writing.
   */
  public Path createTRA(String type, String description, String name) throws IOException {
    // create the annotation file
    String annotationTemplate =
        String.format("{\"type\": {\"%s\": {}}, \"description\": \"%s\"}", type, description);
    return createTRA(annotationTemplate, name);
  }

  public Path createTRALogcatLog(String description, String name) throws IOException {
    // Use Logcat type(id = 4 https://fburl.com/code/8xzvasdf)
    String annotationTemplate =
        String.format(
            "{\"type\": {\"formatted_log\": {\"log_source\": 4}}, \"description\": \"%s\"}",
            description);
    return createTRA(annotationTemplate, name);
  }

  public Path createTRAPlainTextLog(String description, String name) throws IOException {
    // Use PLAIN_TEXT type(id = 5 https://fburl.com/code/8xzvasdf)
    String annotationTemplate =
        String.format(
            "{\"type\": {\"formatted_log\": {\"log_source\": 5}}, \"description\": \"%s\"}",
            description);
    return createTRA(annotationTemplate, name);
  }

  private Path createTRA(String annotationTemplate, String name) throws IOException {
    /// get TRA directories
    String testArtifactsPath = getenv(TEST_RESULT_ARTIFACTS_ENV);
    String testArtifactsAnnotationsPath = getenv(TEST_RESULT_ARTIFACTS_ANNOTATIONS_ENV);
    if (testArtifactsPath == null || testArtifactsAnnotationsPath == null) {
      System.out.printf("The environment variables for TRA were not provided.\n");
      return null;
    }
    Path annotationsPath =
        Paths.get(testArtifactsAnnotationsPath, String.format("%s.annotation", name));
    Files.write(annotationsPath, annotationTemplate.getBytes(Charset.forName("UTF-8")));

    // create the artifact file
    return Paths.get(testArtifactsPath, name);
  }

  /**
   * Pulls file directory from the device to local directory.
   *
   * @param sourceDir the dir of the source
   * @param destinationDir the dir of the destination
   * @throws Exception exceptions may throw from file operations
   */
  public void pullDir(String sourceDir, String destinationDir) throws Exception {
    if (!directoryExists(sourceDir)) {
      // source dir or one of its parents doesn't exist, nothing to pull.
      System.err.printf("Failed to locate source directory: %s\n", sourceDir);
      return;
    }
    File destinationDirFile = new File(destinationDir);
    if (!destinationDirFile.exists()) {
      destinationDirFile.mkdirs();
    }
    String sourceDirWithContents = sourceDir.endsWith("/") ? sourceDir + "." : sourceDir + "/.";
    transferFile("pull", sourceDirWithContents, destinationDir);
  }

  // push single file
  public void pushFileWithSyncService(String local, String remote) throws Exception {
    pushFile(local, remote);
  }

  // pull single file
  public void pullFileWithSyncService(String remote, String local) throws Exception {
    pullFile(remote, local);
  }

  // Java has no setenv(), so this is needed to be able to overwrite env vars in tests
  // the lint suggests to use EnvVariablesProvider, but the
  // compiler cannot find it
  @SuppressWarnings("PMD.BlacklistedSystemGetenv")
  protected String getenv(String env) {
    return System.getenv(env);
  }

  @SuppressWarnings("PMD.BlacklistedSystemGetenv")
  protected Map<String, String> getenv() {
    return System.getenv();
  }

  public boolean directoryExists(final String dirPath) throws Exception {
    String output =
        adbUtils.executeAdbShellCommand(
            String.format("test -d %s && echo exists", dirPath),
            androidDevice.getSerialNumber(),
            true);
    return output.contains("exists");
  }

  /**
   * Execute adb shell command. Public wrapper around executeAdbShellCommand for use by report
   * layers in other packages.
   */
  public String runShellCommand(String command) throws Exception {
    return executeAdbShellCommand(command);
  }

  protected void transferFile(String operation, String source, String destination)
      throws Exception {
    adbUtils.executeAdbCommand(
        operation + " " + source + " " + destination, androidDevice.getSerialNumber(), false);
  }

  private void pullFile(String remotePath, String localPath) throws Exception {
    transferFile("pull", remotePath, localPath);
  }

  private void pushFile(String localPath, String remotePath) throws Exception {
    transferFile("push", localPath, remotePath);
  }

  @FunctionalInterface
  protected interface FilePusher {
    void pushFile(String localPath, String remotePath) throws Exception;
  }

  /** Copy all local files to the remote device location */
  protected void syncExopackageDir(Path localBase) throws Exception {
    syncExopackageDir(localBase, this::pushFile);
  }

  /** Copy all local files to the remote device location (static version for testing) */
  protected static void syncExopackageDir(Path localBase, FilePusher pusher) throws Exception {
    String metadataContents = new String(Files.readAllBytes(localBase.resolve("metadata.txt")));
    Path remoteBase = Paths.get(metadataContents.trim());
    // TODO: speed this up by checking for already installed items
    // TODO: speed this up by only installing ABI-compatible shared-objects
    try (Stream<Path> paths = Files.walk(localBase, FileVisitOption.FOLLOW_LINKS)) {
      Iterable<Path> localFiles = () -> paths.filter(p -> !Files.isDirectory(p)).iterator();
      for (Path p : localFiles) {
        Path localSuffix = localBase.relativize(p);
        Path fullRemotePath = remoteBase.resolve(localSuffix);
        // Remote path is always a unix path
        pusher.pushFile(p.toString(), fullRemotePath.toString().replace('\\', '/'));
      }
    }
  }

  private String getAppScopedStoragePath(
      String packageName,
      String targetPackageName,
      boolean isSelfInstrumenting,
      String folderName,
      String fileName) {
    String basePath =
        getAppScopedStoragePath(packageName, targetPackageName, isSelfInstrumenting, folderName);
    if (basePath == null) {
      return null;
    }
    return Paths.get(basePath, fileName).toString();
  }

  private String getAppScopedStoragePath(
      String packageName,
      String targetPackageName,
      boolean isSelfInstrumenting,
      String folderName) {
    int sdkVersion = getSdkVersion();
    // App Scoped Storage was introduced in Android 11
    if (sdkVersion == -1 || sdkVersion < 30) {
      return null;
    }

    String artifactsDirTemplate =
        INSTRUMENTATION_TEST_APP_SCOPED_API_30_PLUS_ARTIFACTS_DIR_TEMPLATE;
    if (sdkVersion >= 33) {
      artifactsDirTemplate = INSTRUMENTATION_TEST_APP_SCOPED_API_34_PLUS_ARTIFACTS_DIR_TEMPLATE;
    }

    String devicePathPackage = packageName;
    if (!isSelfInstrumenting && targetPackageName != null) {
      devicePathPackage = targetPackageName;
    }

    return String.format(artifactsDirTemplate, devicePathPackage, folderName);
  }

  private int getSdkVersion() {
    try {
      String sdk = androidDevice.getProperty("ro.build.version.sdk");
      return Integer.parseInt(sdk);
    } catch (NumberFormatException e) {
      System.err.printf("Unable to determine SDK version for device: %s\n", e);
    } catch (Exception e) {
      System.err.printf("Unable to get SDK version property: %s\n", e);
    }
    return -1;
  }

  private AnimationScales getAnimationScales() throws Exception {
    Function<String, Float> converter =
        s -> {
          if (s == null || "null".equals(s.trim())) {
            return 0.0f;
          }
          return Float.parseFloat(s);
        };
    float windowAnimationScale =
        converter.apply(executeAdbShellCommand("settings get global window_animation_scale"));
    float transitionAnimationScale =
        converter.apply(executeAdbShellCommand("settings get global transition_animation_scale"));
    float animatorDurationScale =
        converter.apply(executeAdbShellCommand("settings get global animator_duration_scale"));

    return new AnimationScales(
        windowAnimationScale, transitionAnimationScale, animatorDurationScale);
  }

  private void setAnimationScales(AnimationScales animationScales) throws Exception {
    executeAdbShellCommand(
        "settings put global window_animation_scale " + animationScales.windowAnimationScale);
    executeAdbShellCommand(
        "settings put global transition_animation_scale "
            + animationScales.transitionAnimationScale);
    executeAdbShellCommand(
        "settings put global animator_duration_scale " + animationScales.animatorDurationScale);
  }

  // VisibleForTesting
  static void setTrimLine(RemoteAndroidTestRunner runner, boolean value) {
    try {
      Field mParserField = RemoteAndroidTestRunner.class.getDeclaredField("mParser");
      mParserField.setAccessible(true);
      MultiLineReceiver multiLineReceiver = (MultiLineReceiver) mParserField.get(runner);
      multiLineReceiver.setTrimLine(value);
    } catch (NoSuchFieldException | IllegalAccessException e) {
      throw new RuntimeException(e);
    }
  }
}
