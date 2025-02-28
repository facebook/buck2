/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util;

import com.facebook.buck.core.util.immutables.BuckStyleValue;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import java.nio.file.Path;
import java.util.Optional;

/** Value type passed to {@link ProcessExecutor} to launch a process. */
@BuckStyleValue
public abstract class ProcessExecutorParams {

  public static ProcessExecutorParams ofCommand(String... args) {
    return builder().addCommand(args).build();
  }

  /** The command and arguments to launch. */
  public abstract ImmutableList<String> getCommand();

  /** If present, the current working directory for the launched process. */
  public abstract Optional<Path> getDirectory();

  /**
   * If present, the map of environment variables used for the launched process. Otherwise, inherits
   * the current process's environment.
   */
  public abstract Optional<ImmutableMap<String, String>> getEnvironment();

  /**
   * If present, redirects stdout for the process to this location. Otherwise, opens a pipe for
   * stdout.
   */
  public abstract Optional<ProcessBuilder.Redirect> getRedirectInput();

  /**
   * If present, redirects stdin for the process to this location. Otherwise, opens a pipe for
   * stdin.
   */
  public abstract Optional<ProcessBuilder.Redirect> getRedirectOutput();

  /**
   * If present, redirects stderr for the process to this location. Otherwise, opens a pipe for
   * stderr.
   */
  public abstract Optional<ProcessBuilder.Redirect> getRedirectError();

  /*
   * If true, redirects stderr for the process to stdout.
   */
  public abstract Optional<Boolean> getRedirectErrorStream();

  public ProcessExecutorParams withRedirectError(ProcessBuilder.Redirect redirectError) {
    return builder().from(this).setRedirectError(redirectError).build();
  }

  public ProcessExecutorParams withEnvironment(ImmutableMap<String, String> environment) {
    return builder().from(this).setEnvironment(environment).build();
  }

  public static Builder builder() {
    return new Builder();
  }

  public static class Builder extends ImmutableProcessExecutorParams.Builder {}
}
