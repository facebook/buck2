@file:JvmName("ClassAbiWriterFactory")

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.cd.workertool.postexecutors

import com.facebook.buck.core.filesystems.AbsPath
import com.facebook.buck.jvm.java.abi.StubJar
import java.nio.file.Path
import java.nio.file.Paths

class ClassAbiWriter(
    private val libraryStubJar: StubJar,
    private val jvmAbiGenDir: AbsPath,
    private val abiJar: AbsPath,
) {

  fun execute() {
    libraryStubJar.apply {
      setExistingAbiJar(jvmAbiGenDir)
      writeTo(abiJar)
    }
  }

  companion object {

    private val root: AbsPath = AbsPath.of(Paths.get(".").toAbsolutePath().normalize())

    @JvmStatic
    fun create(libraryJar: Path?, jvmAbiGenDir: Path?, abiJar: Path?): ClassAbiWriter =
        ClassAbiWriter(
            StubJar(root.resolve(requireNotNull(libraryJar))),
            root.resolve(requireNotNull(jvmAbiGenDir)),
            root.resolve(requireNotNull(abiJar)),
        )
  }
}
