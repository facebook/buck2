/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd.command

import com.facebook.buck.cd.model.common.PostBuildParams as PostBuildParamsProto
import java.nio.file.Path
import java.nio.file.Paths

class PostBuildParams(
    val libraryJar: Path?,
    val abiJar: Path?,
    val jvmAbiGen: Path?,
    val abiOutputDir: Path?,
    val usedClassesPaths: List<Path>,
    val depFile: Path?,
    val jarToJarDirMap: Path?,
    val optionalDirsPaths: List<Path>,
    val incrementalStateDir: Path?,
    val shouldCreateClassAbi: Boolean,
    val usedJarsPath: Path?,
) {
  companion object {
    fun fromProto(model: PostBuildParamsProto): PostBuildParams =
        PostBuildParams(
            model.libraryJar.takeIf { it.isNotEmpty() }?.let(Paths::get),
            model.abiJar.takeIf { it.isNotEmpty() }?.let(Paths::get),
            model.jvmAbiGen.takeIf { it.isNotEmpty() }?.let(Paths::get),
            model.abiOutputDir.takeIf { it.isNotEmpty() }?.let(Paths::get),
            model.usedClassesList?.map(Paths::get).orEmpty(),
            model.depFile.takeIf { it.isNotEmpty() }?.let(Paths::get),
            model.jarToJarDirMap.takeIf { it.isNotEmpty() }?.let(Paths::get),
            model.optionalDirsList?.map(Paths::get).orEmpty(),
            model.incrementalStateDir.takeIf { it.isNotEmpty() }?.let(Paths::get),
            model.shouldCreateClassAbi,
            model.usedJarsFile.takeIf { it.isNotEmpty() }?.let(Paths::get))
  }
}
