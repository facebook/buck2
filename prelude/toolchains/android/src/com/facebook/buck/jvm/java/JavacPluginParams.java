/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.core.util.immutables.BuckStyleValueWithBuilder;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSortedSet;
import java.net.URL;
import org.immutables.value.Value;

/**
 * Information for javac plugins (includes annotation processors).
 *
 * <p>Javac Plugins involves a set of plugin properties, their classpath(s), and a few other
 * command-line options for javac. We want to be able to specify all this various information in a
 * BUCK configuration file and use it when we generate the javac command. This facilitates threading
 * the information through buck in a more descriptive package rather than passing all the components
 * separately.
 */
@BuckStyleValueWithBuilder
public abstract class JavacPluginParams {

  public static final JavacPluginParams EMPTY = builder().build();

  public abstract ImmutableList<ResolvedJavacPluginProperties> getPluginProperties();

  @Value.NaturalOrder
  public abstract ImmutableSortedSet<String> getParameters();

  public boolean isEmpty() {
    return getPluginProperties().isEmpty() && getParameters().isEmpty();
  }

  public static Builder builder() {
    return new Builder();
  }

  /** Resolves classpath to a list of URL */
  public ImmutableList<URL> toUrlClasspath(AbsPath relPathRoot) {
    ImmutableList.Builder<URL> builder = ImmutableList.builder();
    for (ResolvedJavacPluginProperties plugin : getPluginProperties()) {
      builder.addAll(plugin.toUrlClasspath(relPathRoot));
    }
    return builder.build();
  }

  /** A customized Builder for JavacPluginParams. */
  @org.immutables.builder.Builder.AccessibleFields
  public static class Builder extends ImmutableJavacPluginParams.Builder {}
}
