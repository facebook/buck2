/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.applicability.common

import android.annotation.SuppressLint
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.Checker
import com.facebook.kotlin.compilerplugins.kosabi.applicability.common.checkers.Violation
import java.io.BufferedReader
import java.io.File
import java.io.FileWriter
import java.io.InputStreamReader
import java.nio.charset.StandardCharsets
import kotlin.io.path.Path
import org.jetbrains.kotlin.com.intellij.openapi.editor.Document
import org.jetbrains.kotlin.com.intellij.openapi.vfs.VirtualFile
import org.jetbrains.kotlin.com.intellij.psi.PsiDocumentManager
import org.jetbrains.kotlin.psi.KtFile

fun writeCheckerOutput(
    checkerNameToDetectedFiles: Map<Checker, Set<String>>,
    outputDirectory: String,
) {
  val dir = File(outputDirectory)
  if (!dir.exists()) {
    dir.mkdir()
  }

  checkerNameToDetectedFiles.forEach { (checker, detectedFiles) ->
    val outputFile = File(dir, "${checker.name}.out")
    FileWriter(outputFile, true).use { writer -> detectedFiles.forEach { writer.write("$it\n") } }
  }
}

fun addViolationsPaths(
    psiDocumentManager: PsiDocumentManager,
    file: KtFile,
    violationsPaths: MutableSet<String>,
    violations: List<Violation>,
) {
  var document: Document? = null
  try {
    document = psiDocumentManager.getDocument(file)
  } catch (_: UnsupportedOperationException) {
    /* some generated files such as ultralight are not supported
    in psi getDocument method */
  }

  violations.forEach { violation ->
    if (document != null) {
      val lineNumber = document.getLineNumber(violation.textOffset)
      val lineStartOffset = document.getLineStartOffset(lineNumber)
      val col = violation.textOffset - lineStartOffset
      violationsPaths.add("${file.virtualFilePath}:${lineNumber+1}:${col+1} ${violation.extra}")
    } else {
      val lineNumber = readLineNumberFromBuffer(file.virtualFile, violation.textOffset)
      violationsPaths.add("${file.virtualFilePath}:${lineNumber} ${violation.extra}")
    }
  }
}

/**
 * Exclude generated files from KSP framework, ex:
 * /Users/username/fbsource/fbandroid/java/com/test/EditedVirtualFile.kt
 * /Users/username/fbsource/fbandroid/java/com/test/SomeFile.kt
 * /GeneratedSource33c9064660f6f10f36f8356dbc4a42f3397c8d3b378b4618831831de23f6295.kt
 *
 * We skip the generated sources by recognizing its path, some framework will rewrite the sources
 * into a LightVirtualFile.
 */
@SuppressLint("NewApi")
fun filterOutGeneratedFiles(files: Collection<KtFile>): List<KtFile> =
    files.filter { Path(it.virtualFilePath).parent != Path("/") }

private fun readLineNumberFromBuffer(virtualFile: VirtualFile, textOffset: Int): Int {
  // Avoid empty string case
  if (textOffset == 0) return 1
  val reader = BufferedReader(InputStreamReader(virtualFile.inputStream, StandardCharsets.UTF_8))
  var lineNumber = 0
  var offset = 0
  while (offset <= textOffset) {
    offset += reader.readLine().length + 1
    lineNumber++
  }
  return lineNumber
}
