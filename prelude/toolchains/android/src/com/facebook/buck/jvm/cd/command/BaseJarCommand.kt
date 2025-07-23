/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd.command

import com.facebook.buck.cd.model.java.AbiGenerationMode
import com.facebook.buck.cd.model.java.BaseJarCommand as ProtoBaseJarCommand
import com.facebook.buck.core.filesystems.AbsPath
import com.facebook.buck.core.filesystems.RelPath
import com.facebook.buck.jvm.cd.serialization.AbsPathSerializer
import com.facebook.buck.jvm.cd.serialization.RelPathSerializer
import com.facebook.buck.jvm.cd.serialization.java.BuildTargetValueSerializer
import com.facebook.buck.jvm.cd.serialization.java.CompilerOutputPathsValueSerializer
import com.facebook.buck.jvm.cd.serialization.java.JarParametersSerializer
import com.facebook.buck.jvm.cd.serialization.java.ResolvedJavacOptionsSerializer
import com.facebook.buck.jvm.cd.serialization.java.ResolvedJavacSerializer
import com.facebook.buck.jvm.core.BuildTargetValue
import com.facebook.buck.jvm.java.CompilerOutputPathsValue
import com.facebook.buck.jvm.java.JarParameters
import com.facebook.buck.jvm.java.ResolvedJavac
import com.facebook.buck.jvm.java.ResolvedJavacOptions
import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableMap
import com.google.common.collect.ImmutableSortedSet
import java.util.Optional

class BaseJarCommand(
    val abiCompatibilityMode: AbiGenerationMode,
    val abiGenerationMode: AbiGenerationMode,
    val isRequiredForSourceOnlyAbi: Boolean,
    val trackClassUsage: Boolean,
    val compilerOutputPathsValue: CompilerOutputPathsValue,
    val compileTimeClasspathPaths: ImmutableList<RelPath>,
    val compileTimeClasspathSnapshotPathsMap: ImmutableMap<RelPath, RelPath>,
    val javaSrcs: ImmutableSortedSet<RelPath>,
    val resourcesMap: ImmutableMap<RelPath, RelPath>,
    val jarParameters: JarParameters?,
    val buildCellRootPath: AbsPath,
    val resolvedJavac: ResolvedJavac,
    val resolvedJavacOptions: ResolvedJavacOptions,
    val buildTargetValue: BuildTargetValue,
    val buckOut: RelPath,
    val pathToClasses: RelPath?,
    val annotationPath: RelPath?
) {

  companion object {
    fun fromProto(model: ProtoBaseJarCommand, scratchDir: Optional<RelPath>): BaseJarCommand {
      return BaseJarCommand(
          model.abiCompatibilityMode,
          model.abiGenerationMode,
          model.trackClassUsage,
          model.trackClassUsage,
          CompilerOutputPathsValueSerializer.deserialize(model.outputPathsValue, scratchDir),
          RelPathSerializer.toListOfRelPath(model.compileTimeClasspathPathsList),
          RelPathSerializer.spaceSeparatedListEntriestoMap(
              model.compileTimeClasspathSnapshotPathsList),
          RelPathSerializer.toSortedSetOfRelPath(model.getJavaSrcsList()),
          RelPathSerializer.toResourceMap(model.resourcesMapList),
          if (model.hasJarParameters()) JarParametersSerializer.deserialize(model.jarParameters)
          else null,
          AbsPathSerializer.deserialize(""),
          ResolvedJavacSerializer.deserialize(model.resolvedJavac),
          ResolvedJavacOptionsSerializer.deserialize(model.resolvedJavacOptions),
          BuildTargetValueSerializer.deserialize(model.buildTargetValue),
          RelPathSerializer.deserialize(model.configuredBuckOut),
          RelPathSerializer.deserialize(model.pathToClasses),
          RelPathSerializer.deserialize(model.annotationsPath))
    }
  }
}
