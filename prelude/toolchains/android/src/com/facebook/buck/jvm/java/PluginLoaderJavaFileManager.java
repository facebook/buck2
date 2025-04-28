/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;

public class PluginLoaderJavaFileManager extends ForwardingStandardJavaFileManager
    implements StandardJavaFileManager {

  private final JavacExecutionContext context;
  private final PluginFactory pluginFactory;
  private final JavacPluginParams javacPlugins;

  /**
   * Creates a new instance of ForwardingJavaFileManager.
   *
   * @param fileManager delegate to this file manager
   * @param javacPlugins
   */
  protected PluginLoaderJavaFileManager(
      JavacExecutionContext context,
      StandardJavaFileManager fileManager,
      PluginFactory pluginFactory,
      JavacPluginParams javacPlugins) {
    super(fileManager);
    this.context = context;
    this.pluginFactory = pluginFactory;
    this.javacPlugins = javacPlugins;
  }

  @Override
  public ClassLoader getClassLoader(Location location) {
    // We only provide the shared classloader if there are plugins defined.
    if (StandardLocation.ANNOTATION_PROCESSOR_PATH.equals(location)
        && javacPlugins.getPluginProperties().size() > 0) {
      return pluginFactory.getClassLoaderForProcessorGroups(
          javacPlugins, context.getRuleCellRoot());
    }
    return super.getClassLoader(location);
  }
}
