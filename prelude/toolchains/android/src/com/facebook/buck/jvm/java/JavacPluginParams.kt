/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java

import com.facebook.buck.core.filesystems.AbsPath
import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableSortedSet
import java.net.URL

/**
 * Information for javac plugins (includes annotation processors).
 *
 * Javac Plugins involves a set of plugin properties, their classpath(s), and a few other
 * command-line options for javac. We want to be able to specify all this various information in a
 * BUCK configuration file and use it when we generate the javac command. This facilitates threading
 * the information through buck in a more descriptive package rather than passing all the components
 * separately.
 */
data class JavacPluginParams(
    val pluginProperties: ImmutableList<ResolvedJavacPluginProperties>,
    val parameters: ImmutableSortedSet<String>
) {
  val isEmpty: Boolean
    get() = pluginProperties.isEmpty() && parameters.isEmpty()

  /** Resolves classpath to a list of URL */
  fun toUrlClasspath(relPathRoot: AbsPath): ImmutableList<URL> {
    val builder = ImmutableList.builder<URL>()
    for (plugin in pluginProperties) {
      builder.addAll(plugin.toUrlClasspath(relPathRoot))
    }
    return builder.build()
  }

  companion object {
    @JvmField
    val EMPTY: JavacPluginParams = JavacPluginParams(ImmutableList.of(), ImmutableSortedSet.of())
  }
}
