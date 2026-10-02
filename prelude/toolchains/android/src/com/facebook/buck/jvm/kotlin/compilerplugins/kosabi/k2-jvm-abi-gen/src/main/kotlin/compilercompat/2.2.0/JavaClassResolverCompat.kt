/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

@file:Suppress("PackageLocationMismatch")

package com.facebook

import org.jetbrains.kotlin.fir.declarations.FirClass
import org.jetbrains.kotlin.fir.declarations.utils.classId
import org.jetbrains.kotlin.fir.java.FirJavaAwareSymbolProvider
import org.jetbrains.kotlin.fir.moduleData
import org.jetbrains.kotlin.fir.resolve.providers.impl.FirCachingCompositeSymbolProvider
import org.jetbrains.kotlin.fir.resolve.providers.symbolProvider
import org.jetbrains.kotlin.load.java.structure.impl.VirtualFileBoundJavaClass

// Workaround for
// https://youtrack.jetbrains.com/issue/KT-60555/K2.-FirJavaClass-source-field-is-null:
// library sessions never register the session-level javaSymbolProvider, so scan providers.
fun FirClass.toBinaryJavaClassCompat(): VirtualFileBoundJavaClass {
  val symbolProvider = moduleData.session.symbolProvider
  val composite =
      symbolProvider as? FirCachingCompositeSymbolProvider
          ?: error(
              "KT-60555 workaround needs FirCachingCompositeSymbolProvider, got ${symbolProvider::class}",
          )
  val javaAware =
      composite.providers.filterIsInstance<FirJavaAwareSymbolProvider>().firstOrNull()
          ?: error(
              "KT-60555 workaround needs a FirJavaAwareSymbolProvider, found none among ${composite.providers.size} providers",
          )
  val binaryClass =
      javaAware.javaFacade.findClass(classId)
          ?: error("KT-60555 workaround: FirJavaFacade cannot resolve $classId")
  if (binaryClass !is VirtualFileBoundJavaClass) {
    error("Unsupported kind of a JavaClass: ${binaryClass::class}")
  }
  return binaryClass
}
