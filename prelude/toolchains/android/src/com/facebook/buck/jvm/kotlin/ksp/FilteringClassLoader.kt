/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.ksp

/**
 * This class loader selectively routes classes from an allowlist to a specific class loader and
 * everything else to another class loader. This can be used to isolate some loaded classes from
 * most of the classes already loaded by the parent JVM, while letting others (in our case, the
 * tracing agent classes) get reused.
 */
class FilteringClassLoader(
    private val allowlistedParent: ClassLoader,
    private val blocklistedParent: ClassLoader,
    private vararg val allowlist: String,
) : ClassLoader(allowlistedParent) {
  override fun loadClass(name: String, resolve: Boolean): Class<*> {
    if (allowlist.any { name.startsWith(it) }) {
      return allowlistedParent.loadClass(name)
    }

    return blocklistedParent.loadClass(name)
  }
}
