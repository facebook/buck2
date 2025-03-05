/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import java.nio.file.Path;
import java.util.Objects;
import java.util.Optional;

/** Value type passed to {@link ProcessExecutor} to launch a process. */
public class ProcessExecutorParams {

  private final ImmutableList<String> command;
  private final Optional<ImmutableMap<String, String>> environment;
  private final Optional<Path> directory;

  public ProcessExecutorParams(ImmutableList<String> command) {
    this(command, Optional.empty(), Optional.empty());
  }

  public ProcessExecutorParams(
      ImmutableList<String> command,
      Optional<ImmutableMap<String, String>> environment,
      Optional<Path> directory) {
    this.command = command;
    this.environment = environment;
    this.directory = directory;
  }

  public static ProcessExecutorParams ofCommand(String... args) {
    return new ProcessExecutorParams(ImmutableList.copyOf(args));
  }

  /** The command and arguments to launch. */
  public ImmutableList<String> getCommand() {
    return command;
  }

  /** If present, the current working directory for the launched process. */
  public Optional<Path> getDirectory() {
    return directory;
  }

  /**
   * If present, the map of environment variables used for the launched process. Otherwise, inherits
   * the current process's environment.
   */
  public Optional<ImmutableMap<String, String>> getEnvironment() {
    return environment;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    ProcessExecutorParams that = (ProcessExecutorParams) o;
    return Objects.equals(command, that.command)
        && Objects.equals(environment, that.environment)
        && Objects.equals(directory, that.directory);
  }

  @Override
  public int hashCode() {
    return Objects.hash(command, environment, directory);
  }
}
