/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.buildtools

import com.facebook.buck.jvm.java.javax.SynchronizedToolProvider
import org.jetbrains.kotlin.buildtools.api.CompilationService
import org.jetbrains.kotlin.buildtools.api.ExperimentalBuildToolsApi
import org.jetbrains.kotlin.buildtools.api.SharedApiClassesClassLoader

@OptIn(ExperimentalBuildToolsApi::class)
internal object SharedApiClassesClassLoaderProvider {

  val sharedSynchronizedApiClassesClassLoader: SharedSynchronizedApiClassesClassLoader =
      SharedSynchronizedApiClassesClassLoader(
          SharedApiClassesClassLoaderProvider::class.java.classLoader,
          CompilationService::class.java.`package`.name)
}

/**
 * Customized implementation of the [SharedApiClassesClassLoader], which allows us to use the
 * [SynchronizedToolProvider] to obtain a parent class loader. See:
 * https://github.com/JetBrains/kotlin/blob/6355fbbcb544c876b161426ef84d34bdb88b37bc/compiler/build-tools/kotlin-build-tools-api/src/main/kotlin/org/jetbrains/kotlin/buildtools/api/SharedApiClassesClassLoader.kt#L34
 */
internal class SharedSynchronizedApiClassesClassLoader(
    // A name resolution for old Kotlin compiler versions.
    // Kotlin 1.4 can not resolve the name of the parent class loader.
    @get:JvmName("parentClassLoader") private val parent: ClassLoader,
    private val allowedPackage: String,
) : ClassLoader(SynchronizedToolProvider.getSystemToolClassLoader()) {
  override fun loadClass(name: String, resolve: Boolean): Class<*> {
    return if (name.startsWith(allowedPackage)) {
      parent.loadClass(name)
    } else {
      super.loadClass(name, resolve)
    }
  }
}
