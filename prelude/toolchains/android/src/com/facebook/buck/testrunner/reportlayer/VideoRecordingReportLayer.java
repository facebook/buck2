/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.testrunner.reportlayer;

import com.android.ddmlib.IDevice;
import com.facebook.buck.testrunner.InstrumentationTestRunner;
import java.io.File;
import java.io.FileWriter;
import java.nio.file.Path;

/**
 * report layer to start a process to record video of tests before they run and collect the video as
 * TRA after tests finish
 */
public class VideoRecordingReportLayer extends ReportLayer {

  private static final String VIDEO_RECORDING_STORAGE_PATH =
      "/sdcard/test_result/video_recordings/";
  private static final String VIDEO_RECORDING_SCRIPT_NAME = "test-video-record.sh";
  private static final String VIDEO_RECORDING_FILE_NAME = "test-video-record.mp4";

  public static final String ARG = "--record-video";

  private Process videoRecordingProcess;

  public VideoRecordingReportLayer(InstrumentationTestRunner runner) {
    super(runner);
  }

  @Override
  public void initialize() {
    this.videoRecordingProcess = this.startVideoRecording(runner.getDevice());
  }

  @Override
  public void report() {
    this.collectVideoRecording(runner.getDevice());
  }

  private String getVideoRecordingStoragePath() {
    return String.format(VIDEO_RECORDING_STORAGE_PATH, this.runner.getPackageName());
  }

  private Process startVideoRecording(IDevice device) {
    try {
      File scriptFile = File.createTempFile("video-recording-", ".sh");
      FileWriter scriptFileWriter = new FileWriter(scriptFile);
      // create local script file
      scriptFileWriter.write(
          String.join(
              "\n",
              "exec 2>&1  # merge stdout and stderr",
              "set -xue",
              String.format(
                  "screenrecord --verbose --bugreport %s &",
                  String.format(
                      "%s/%s", this.getVideoRecordingStoragePath(), VIDEO_RECORDING_FILE_NAME)),
              "pid=$!  # screenrecord pid",
              "read  # wait for stopword (any input)",
              "kill -INT $pid  # request screenrecord to stop gracefully",
              "wait $pid"));
      scriptFile.deleteOnExit();
      scriptFileWriter.close();
      // push script to device
      String remoteScriptPath =
          String.format("%s/%s", this.getVideoRecordingStoragePath(), VIDEO_RECORDING_SCRIPT_NAME);
      this.runner.pushFileWithSyncService(device, scriptFile.getAbsolutePath(), remoteScriptPath);
      // start recording process
      return this.runner.exec(String.format("adb shell sh %s", remoteScriptPath));
    } catch (Exception e) {
      System.err.printf("Failed to start video recording process with error: %s\n", e);
      return null;
    }
  }

  private void collectVideoRecording(IDevice device) {
    try {
      // stop video recording
      videoRecordingProcess.getOutputStream().write("STOP".getBytes());
      videoRecordingProcess.getOutputStream().write(System.lineSeparator().getBytes());
      videoRecordingProcess.getOutputStream().flush();
      int exitCode = videoRecordingProcess.waitFor();
      if (exitCode != 0) {
        throw new Exception(
            String.format("Failed to stop video recording process with exit code %d", exitCode));
      }
      // pull file to TRA
      Path video_artifact =
          this.runner.createTRA(
              "video_recording_test_artifact", "video recording", VIDEO_RECORDING_FILE_NAME);
      if (null == video_artifact) {
        throw new Exception("Failed to create TRA for vide recording");
      }
      this.runner.pullFileWithSyncService(
          device,
          String.format("%s/%s", this.getVideoRecordingStoragePath(), VIDEO_RECORDING_FILE_NAME),
          video_artifact.toString());
    } catch (Exception e) {
      System.err.printf("Failed to collect video recording process with error: %s", e);
    }
  }
}
