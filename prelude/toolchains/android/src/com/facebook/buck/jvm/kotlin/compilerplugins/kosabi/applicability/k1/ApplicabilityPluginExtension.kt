/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.k1

import com.facebook.kotlin.compilerplugins.common.hasTypesForAnyFiles
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.KotlinSourceOnlyAbiApplicabilityException
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.PROJECT_CHECKERS
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.addViolationsPaths
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.Checker
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.FeatureChecker
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.filterOutGeneratedFiles
import org.jetbrains.kotlin.analyzer.AnalysisResult
import org.jetbrains.kotlin.cli.common.CLIConfigurationKeys
import org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity
import org.jetbrains.kotlin.cli.common.messages.MessageCollector
import org.jetbrains.kotlin.com.intellij.openapi.project.Project
import org.jetbrains.kotlin.com.intellij.psi.PsiDocumentManager
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.descriptors.ModuleDescriptor
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.resolve.BindingTrace
import org.jetbrains.kotlin.resolve.jvm.extensions.AnalysisHandlerExtension

/** An applicability compiler plugin for Kosabi */
@OptIn(org.jetbrains.kotlin.compiler.plugin.ExperimentalCompilerApi::class)
class ApplicabilityPluginExtension(
    compilerConfiguration: CompilerConfiguration,
    private val checkers: List<FeatureChecker>
) : AnalysisHandlerExtension {
  private val compilerConfiguration: CompilerConfiguration = compilerConfiguration.copy()
  private val messageCollector =
      compilerConfiguration.get(CLIConfigurationKeys.MESSAGE_COLLECTOR_KEY, MessageCollector.NONE)

  override fun analysisCompleted(
      project: Project,
      module: ModuleDescriptor,
      bindingTrace: BindingTrace,
      files: Collection<KtFile>,
  ): AnalysisResult? {
    val checkerNameToViolationsPath =
        sortedMapOf<Checker, MutableSet<String>>(compareBy(Checker::name))

    /**
     * When KAPT, KSP or any Analysis handler extension returns non null result (changing source
     * code), analysisCompleted is called again. Before the last run, the types are not ready yet,
     * so we skip applicability checks until types are available.
     */
    if (!hasTypesForAnyFiles(bindingTrace.bindingContext, files)) {
      return null
    }

    PROJECT_CHECKERS.forEach { checker ->
      val extras = checker.findViolations(compilerConfiguration)
      if (extras.isNotEmpty()) {
        checkerNameToViolationsPath.getOrPut(checker) { extras.toMutableSet() }
      }
    }

    val psiDocumentManager = PsiDocumentManager.getInstance(project)
    val sourceFiles = filterOutGeneratedFiles(files)

    sourceFiles.forEach { file ->
      checkers.forEach { checker ->
        val violations = checker.findViolations(file, bindingTrace.bindingContext)
        if (violations.isNotEmpty()) {
          val violationsPaths = checkerNameToViolationsPath.getOrPut(checker) { mutableSetOf() }
          addViolationsPaths(psiDocumentManager, file, violationsPaths, violations)
        }
      }
    }

    if (checkerNameToViolationsPath.isEmpty()) {
      return null
    }

    val failedApplicabilityException =
        KotlinSourceOnlyAbiApplicabilityException(checkerNameToViolationsPath)

    messageCollector.report(
        CompilerMessageSeverity.ERROR,
        "Kosabi applicability failed:\n${failedApplicabilityException.message}")

    return AnalysisResult.compilationError(bindingTrace.bindingContext)
  }
}
