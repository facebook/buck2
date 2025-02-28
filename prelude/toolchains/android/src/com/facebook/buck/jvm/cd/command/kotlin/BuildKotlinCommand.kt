/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.cd.command.kotlin

import com.facebook.buck.cd.model.kotlin.BuildCommand as ProtoBuildCommand
import com.facebook.buck.core.filesystems.RelPath
import com.facebook.buck.jvm.cd.command.BaseJarCommand
import com.facebook.buck.jvm.cd.command.BuildMode
import com.facebook.buck.jvm.cd.serialization.kotlin.KotlinExtraParamsSerializer
import java.util.Optional
import java.util.regex.Pattern

class BuildKotlinCommand(
    val kotlinExtraParams: KotlinExtraParams,
    val baseJarCommand: BaseJarCommand,
    val buildMode: BuildMode,
) {

  val LANGUAGE_VERSION_REGEX: Pattern = Pattern.compile("-language-version=\\d+\\.\\d+")

  fun getCompilerVersion(): String {
    return getLanguageVersion(kotlinExtraParams.extraKotlincArguments)
  }

  fun getLanguageVersion(args: List<String>): String {
    for (arg in args) {
      val matcher = LANGUAGE_VERSION_REGEX.matcher(arg)
      if (matcher.matches()) {
        val splitArg = arg.split("=").dropLastWhile { it.isBlank() }
        check(splitArg.size == 2)
        return splitArg[1]
      }
    }
    return "1.9"
  }

  companion object {
    fun fromProto(model: ProtoBuildCommand, scratchDir: Optional<RelPath>): BuildKotlinCommand {

      val buildMode: BuildMode = BuildMode.fromProto(model.buildMode)
      val baseJarCommand: BaseJarCommand =
          BaseJarCommand.fromProto(model.baseJarCommand, scratchDir)
      val kotlinExtraParams: KotlinExtraParams =
          KotlinExtraParamsSerializer.deserialize(
              model.baseJarCommand.resolvedJavacOptions, model.kotlinExtraParams)

      return BuildKotlinCommand(kotlinExtraParams, baseJarCommand, buildMode)
    }
  }
}
