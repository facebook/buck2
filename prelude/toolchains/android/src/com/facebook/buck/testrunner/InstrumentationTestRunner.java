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

import com.android.ddmlib.AdbCommandRejectedException;
import com.android.ddmlib.DdmPreferences;
import com.android.ddmlib.FileListingService;
import com.android.ddmlib.IDevice;
import com.android.ddmlib.MultiLineReceiver;
import com.android.ddmlib.ShellCommandUnresponsiveException;
import com.android.ddmlib.SyncService;
import com.android.ddmlib.TimeoutException;
import com.android.ddmlib.testrunner.ITestRunListener;
import com.android.ddmlib.testrunner.RemoteAndroidTestRunner;
import com.android.ddmlib.testrunner.TestIdentifier;
import com.facebook.buck.testresultsoutput.TestResultsOutputSender;
import com.facebook.buck.testrunner.reportlayer.LogExtractorReportLayer;
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

  private List<ReportLayer> reportLayers = new ArrayList<>();

  private IDevice device = null;

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
      List<String> apexesToInstall) {
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
    Map<String, String> extraInstrumentationArguments = new HashMap<String, String>();
    Map<String, String> extraFilesToPull = new HashMap<String, String>();
    Map<String, String> extraDirsToPull = new HashMap<String, String>();
    Map<String, String> logExtractors = new HashMap<String, String>();
    String preTestSetupScript = null;
    List<String> extraApksToInstall = new ArrayList<>();

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
          case LogExtractorReportLayer.ARG:
            String logExtractorArg = args[++i];
            String[] logExtractorArgSplit = logExtractorArg.split("=", 2);
            if (logExtractorArgSplit.length != 2) {
              System.err.printf("Not a valid log extractor argument: %s\n", logExtractorArg);
              System.exit(1);
            }
            logExtractors.put(logExtractorArgSplit[0], logExtractorArgSplit[1]);
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
            argsParser.extraApksToInstall);
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

  protected void installPackage(IDevice device, String path) throws Throwable {
    device.installPackage(path, true);
  }

  @SuppressWarnings({"PMD.BlacklistedSystemGetenv", "PMD.BlacklistedDefaultProcessMethod"})
  public void run() throws Throwable {
    IDevice device = getAndroidDevice(deviceArgs.autoRunOnConnectedDevice, deviceArgs.deviceSerial);
    this.device = device;

    if (this.instrumentationApkPath != null) {
      DdmPreferences.setTimeOut(60000);
      installPackage(device, this.instrumentationApkPath);
      if (this.apkUnderTestPath != null) {
        installPackage(device, this.apkUnderTestPath);
      }
    }

    if (this.exopackageLocalPath != null) {
      Path localBase = Paths.get(exopackageLocalPath);
      syncExopackageDir(localBase, device);
    }

    if (this.apkUnderTestExopackageLocalPath != null) {
      Path localBase = Paths.get(apkUnderTestExopackageLocalPath);
      syncExopackageDir(localBase, device);
    }

    String appScopedStorageDeviceArtifactsPath =
        getAppScopedStoragePath(
            device, packageName, targetPackageName, isSelfInstrumenting, "artifacts");
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
              device,
              packageName,
              targetPackageName,
              isSelfInstrumenting,
              "dimensions",
              "dimensions.tsv");
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
      // APEX install sometimes requires root.
      if (!device.root()) {
        throw new RuntimeException("Failed to root device.");
      }

      for (final String apexPath : this.apexesToInstall) {
        System.err.println(String.format("Installing APEX: %s...", apexPath));
        DdmPreferences.setTimeOut(60000);
        // If the APEX is not present, we will install it.
        // If the APEX is already installed, we will update it.
        device.installPackage(apexPath, false, "--apex");
        System.err.println(String.format("APEX installed: %s.", apexPath));
      }
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
            device, packageName, targetPackageName, isSelfInstrumenting, "artifact_annotations");
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
      executeAdbShellCommand("pm clear " + this.packageName, device);
      executeAdbShellCommand("pm clear " + this.targetPackageName, device);
    }

    AnimationScales originalWindowAnimationScales = null;

    if (this.disableAnimations) {
      originalWindowAnimationScales = getAnimationScales(device);
      setAnimationScales(device, new AnimationScales(0f, 0f, 0f));
    }

    // Increase logcat buffer size to 16MB.
    executeAdbShellCommand("logcat -G 16M", device);

    // Clear logcat logs prior to test run
    executeAdbShellCommand("logcat -c", device);

    // Clean up output directories before the run
    for (final String devicePath : this.extraDirsToPull.keySet()) {
      String output = executeAdbShellCommand("rm -fr " + devicePath, device);

      if (directoryExists(devicePath, device)) {
        System.err.printf("Failed to clean up directory %s due to error: %s\n", devicePath, output);
        System.exit(1);
      }

      output = executeAdbShellCommand("mkdir -p " + devicePath, device);
      if (!directoryExists(devicePath, device)) {
        System.err.printf("Failed to create directory %s due to error: %s\n", devicePath, output);
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

      BuckXmlTestRunListener buckXmlListener = new BuckXmlTestRunListener(device);
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

      Optional<TestResultsOutputSender> testResultsOutputSender =
          TestResultsOutputSender.fromDefaultEnvName();
      if (testResultsOutputSender.isPresent()) {
        InstrumentationTpxStandardOutputTestListener tpxListener =
            new InstrumentationTpxStandardOutputTestListener(testResultsOutputSender.get(), device);
        listeners.add(tpxListener);
      }

      runner.run(listeners);

      if (this.disableAnimations) {
        setAnimationScales(device, originalWindowAnimationScales);
      }

      if (this.codeCoverageOutputFile != null || (useJaCoCoCoverage && coverageTempDir != null)) {
        String destCovFileInHost = this.codeCoverageOutputFile;

        if (coverageTempDir != null) {
          (new File(coverageTempDir)).mkdirs();
          destCovFileInHost = coverageTempDir + "/target-package-coverage.ec";
        }

        String covFileInEmu = "/data/data/" + this.targetPackageName + "/files/coverage.ec";
        String covFileInSdcard = "/sdcard/coverage.ec";
        String cpOutput =
            executeAdbShellCommand("su root cp " + covFileInEmu + " " + covFileInSdcard, device);

        System.out.println(cpOutput);
        device.pullFile(covFileInSdcard, destCovFileInHost);

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
        device.pullFile(entry.getKey(), entry.getValue());
      }
      for (Map.Entry<String, String> entry : this.extraDirsToPull.entrySet()) {
        pullDir(device, entry.getKey(), entry.getValue());
      }

    } finally {
      this.collectAdbLogs(device);

      // Restore logcat buffer size to default.
      executeAdbShellCommand("logcat -G 256K", device);

      for (ReportLayer layer : this.reportLayers) {
        layer.report();
      }
      if (this.attemptUninstallInstrumentationApk) {
        // Best effort uninstall from the emulator/device.
        device.uninstallPackage(this.packageName);
      }
      if (this.attemptUninstallApkUnderTest) {
        device.uninstallPackage(this.targetPackageName);
      }
    }
  }

  public Process exec(String command) throws IOException {
    return Runtime.getRuntime().exec(command);
  }

  @Nullable
  private String getDeviceLogcatOutputForBuffer(IDevice device, LogcatBuffer buffer) {
    String adbCommand = String.format("logcat -d -b \"%s\"", buffer.getCliArgument());

    try {
      return executeAdbShellCommand(adbCommand, device);
    } catch (TimeoutException
        | AdbCommandRejectedException
        | ShellCommandUnresponsiveException
        | IOException e) {
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
  private void collectAdbLogs(IDevice device) {
    try {
      for (LogcatBuffer buffer : this.collectedLogcatBuffers) {
        String bufferName = buffer.getCliArgument();
        Path traPath = this.createPathForLogcatBuffer(bufferName);
        if (traPath == null) {
          return;
        }
        String logOutput = getDeviceLogcatOutputForBuffer(device, buffer);
        if (logOutput == null) {
          continue;
        }
        try (FileWriter logWriter = new FileWriter(traPath.toString())) {
          logWriter.write(logOutput);
        }
      }
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
   * pull dir from device to local
   *
   * @param device the device to pull dir from
   * @param sourceDir the dir of the source
   * @param destinationDir the dir of the destination
   * @throws Exception exceptions may throw from file operations
   */
  public void pullDir(IDevice device, String sourceDir, String destinationDir) throws Exception {
    FileListingService listingService = device.getFileListingService();
    FileListingService.FileEntry dir = locateDir(device, listingService, sourceDir);
    if (dir == null) {
      // source dir or one of its parents doesn't exist, nothing to pull.
      System.err.printf("Failed to locate source directory: %s\n", sourceDir);
      return;
    }
    FileListingService.FileEntry[] filesToPull = listingService.getChildrenSync(dir);
    File destinationDirFile = new File(destinationDir);
    if (!destinationDirFile.exists()) {
      destinationDirFile.mkdirs();
    }
    pullWithSyncService(device, filesToPull, destinationDir);
  }

  // the SyncService cannot be mocked. This function gives us something we can overwrite in tests
  protected void pullWithSyncService(
      IDevice device, FileListingService.FileEntry[] filesToPull, String destinationDir)
      throws Exception {
    device.getSyncService().pull(filesToPull, destinationDir, SyncService.getNullProgressMonitor());
  }

  // push single file
  public void pushFileWithSyncService(IDevice device, String local, String remote)
      throws Exception {
    device.getSyncService().pushFile(local, remote, SyncService.getNullProgressMonitor());
  }

  // pull single file
  public void pullFileWithSyncService(IDevice device, String remote, String local)
      throws Exception {
    device.getSyncService().pullFile(remote, local, SyncService.getNullProgressMonitor());
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

  public boolean directoryExists(final String dirPath, final IDevice device)
      throws AdbCommandRejectedException,
          IOException,
          ShellCommandUnresponsiveException,
          TimeoutException {
    return executeAdbShellCommand(String.format("test -d %s && echo exists", dirPath), device)
        .contains("exists");
  }

  private FileListingService.FileEntry locateDir(
      IDevice device, FileListingService listingService, String dirPath) throws Exception {
    if (!directoryExists(dirPath, device)) {
      return null;
    }

    // Construct the file entry to the path manually as we can't dig through the FileListingService
    // as some intermediate directories may be permissioned to disallow listing children.
    FileListingService.FileEntry dir = listingService.getRoot();
    if (dir == null) {
      throw new RuntimeException("Couldn't retrieve root directory from file listing service.");
    }
    for (final String pathSegment : dirPath.split(FileListingService.FILE_SEPARATOR)) {
      if (pathSegment.isEmpty()) {
        // Ignore empty segments.
        continue;
      }

      dir =
          new FileListingService.FileEntry(
              dir, pathSegment, FileListingService.TYPE_DIRECTORY, false);
    }

    return dir;
  }

  /** Copy all local files to the remote device location */
  protected static void syncExopackageDir(Path localBase, IDevice device) throws Exception {
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
        device.pushFile(p.toString(), fullRemotePath.toString().replace('\\', '/'));
      }
    }
  }

  private String getAppScopedStoragePath(
      IDevice device,
      String packageName,
      String targetPackageName,
      boolean isSelfInstrumenting,
      String folderName,
      String fileName) {
    String basePath =
        getAppScopedStoragePath(
            device, packageName, targetPackageName, isSelfInstrumenting, folderName);
    if (basePath == null) {
      return null;
    }
    return Paths.get(basePath, fileName).toString();
  }

  private String getAppScopedStoragePath(
      IDevice device,
      String packageName,
      String targetPackageName,
      boolean isSelfInstrumenting,
      String folderName) {
    int sdkVersion = getSdkVersion(device);
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

  private int getSdkVersion(IDevice device) {
    String sdk = device.getProperty("ro.build.version.sdk");
    try {
      return Integer.parseInt(sdk);
    } catch (NumberFormatException e) {
      System.err.printf("Unable to determine SDK version for device: %s\n", e);
    }
    return -1;
  }

  private AnimationScales getAnimationScales(IDevice device) throws Exception {
    Function<String, Float> converter =
        s -> {
          if (s == null || "null".equals(s.trim())) {
            return 0.0f;
          }
          return Float.parseFloat(s);
        };
    float windowAnimationScale =
        converter.apply(
            executeAdbShellCommand("settings get global window_animation_scale", device));
    float transitionAnimationScale =
        converter.apply(
            executeAdbShellCommand("settings get global transition_animation_scale", device));
    float animatorDurationScale =
        converter.apply(
            executeAdbShellCommand("settings get global animator_duration_scale", device));

    return new AnimationScales(
        windowAnimationScale, transitionAnimationScale, animatorDurationScale);
  }

  private void setAnimationScales(IDevice device, AnimationScales animationScales)
      throws Exception {
    executeAdbShellCommand(
        "settings put global window_animation_scale " + animationScales.windowAnimationScale,
        device);
    executeAdbShellCommand(
        "settings put global transition_animation_scale "
            + animationScales.transitionAnimationScale,
        device);
    executeAdbShellCommand(
        "settings put global animator_duration_scale " + animationScales.animatorDurationScale,
        device);
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
