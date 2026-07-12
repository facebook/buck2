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

import org.jetbrains.kotlin.backend.common.extensions.IrGenerationExtension
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.cli.jvm.compiler.pipeline.ModuleCompilerEnvironment
import org.jetbrains.kotlin.cli.jvm.compiler.pipeline.ModuleCompilerIrBackendInput
import org.jetbrains.kotlin.cli.jvm.compiler.pipeline.ModuleCompilerOutput
import org.jetbrains.kotlin.cli.jvm.compiler.pipeline.convertToIrAndActualizeForJvm
import org.jetbrains.kotlin.cli.jvm.compiler.pipeline.createProjectEnvironment
import org.jetbrains.kotlin.cli.jvm.compiler.pipeline.generateCodeFromIr
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.diagnostics.impl.BaseDiagnosticsCollector
import org.jetbrains.kotlin.fir.backend.jvm.JvmFir2IrExtensions
import org.jetbrains.kotlin.fir.pipeline.FirResult

typealias ModuleCompilerEnvironmentCompat = ModuleCompilerEnvironment

typealias ModuleCompilerIrBackendInputCompat = ModuleCompilerIrBackendInput

typealias ModuleCompilerOutputCompat = ModuleCompilerOutput

fun FirResult.convertToIrAndActualizeForJvmCompat(
    fir2IrExtensions: JvmFir2IrExtensions,
    configuration: CompilerConfiguration,
    diagnosticsReporter: BaseDiagnosticsCollector,
    irGeneratorExtensions: Collection<IrGenerationExtension>,
) = convertToIrAndActualizeForJvm(
    fir2IrExtensions,
    configuration,
    diagnosticsReporter,
    irGeneratorExtensions,
)

fun createProjectEnvironmentCompat(
    configuration: CompilerConfiguration,
    parentDisposable: org.jetbrains.kotlin.com.intellij.openapi.Disposable,
    configFiles: org.jetbrains.kotlin.cli.jvm.compiler.EnvironmentConfigFiles,
    messageCollector: MessageCollector,
) = createProjectEnvironment(configuration, parentDisposable, configFiles, messageCollector)

fun generateCodeFromIrCompat(
    irInput: ModuleCompilerIrBackendInput,
    environment: ModuleCompilerEnvironment,
) = generateCodeFromIr(irInput, environment)
