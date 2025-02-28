/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// @lint-ignore THRIFTCHECKS(deprecated-java-namespace), used wiht 3P thrift library
namespace java com.facebook.buck.jvm.kotlin.cd.scribe.model

# TODO(navidq) make the step below more straight forward
# Prerequisites: make sure you have an external thrift installed (brew install thrift)
# 1. Run the following command from the OD fbcode to copy the contents of KotlinCDLogEntry.thrift
# buck2 run //dsi/logger/cpp/sync_configs_helper:sync_logger_cli -- --fbcode_dir="." logentry KotlinCDLoggerConfig -ll JAVA
# 2. Run the following command to copy KotlinCDLogEntry.thrift file into toolchain
# cp fbcode/dsi/logger/log_entry/KotlinCDLoggerConfig/KotlinCDLogEntry.thrift xplat/toolchains/android/sdk/src/com/facebook/buck/jvm/kotlin/cd/scribe/model
# 3. Update the header of model/KotlinCDLogEntry.thrift (from the license statement until struct definition) to match the content of this file.
# 4: Run the the command below to generate java code
# thrift -gen java -out xplat/toolchains/android/sdk/src xplat/toolchains/android/sdk/src/com/facebook/buck/jvm/kotlin/cd/scribe/model/KotlinCDLogEntry.thrift

# Thrift schema for KotlinCDLoggerConfig.php in www

# Data about KotlinCD actions in android builds
# Use this struct to interact with Scribe APIs in order to Log data.
struct KotlinCDLogEntry {
  # The webserver timestamp at the moment that log() was called.
  4: optional double event_time;
  6: optional string target; # Buck target being compiled

  # Subtarget of the target being compiled, ex so_abi
  7: optional string subtarget;
  8: optional string compiler_version; # Deprecated: Kotlin compiler version

  # The unit timestamp in second for the Scuba Time Column override
  13: optional i64 time;

  # The weight of the record according to current sampling rate
  14: optional i64 weight;
  16: optional string build_uuid; # Unique identifuer of the buck build
  17: optional string execution_platform; # Either of "local" or "remote"
  18: optional i64 num_java_files; # Number of java files in the target
  19: optional i64 num_kotlin_files; # Number of kotlin files in the target

  # True if incremental compilation for the target is enabled, false otherwise
  20: optional bool incremental;
  21: optional string kotlinc_mode; # Compilation mode used
  22: optional string classpath_changes; # KotlinCD action classpath changes status

  # Additional information related to the compilation
  25: optional string extras;
  26: optional string step; # Build rule step being executed

  # Language version for Kotlin source compatibility, passed as -language-version compiler argument
  28: optional string language_version;
}
