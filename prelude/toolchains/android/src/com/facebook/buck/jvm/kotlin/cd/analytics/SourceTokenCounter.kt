/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.kotlin.cd.analytics

import java.nio.file.Files
import java.nio.file.Path
import org.jetbrains.kotlin.lexer.KotlinLexer
import org.jetbrains.kotlin.lexer.KtTokens

object SourceTokenCounter {
  fun countTokens(sourcePaths: List<Path>, rootPath: Path): TokenCounts {
    var kotlinTokens = 0L
    var javaTokens = 0L
    for (path in sourcePaths) {
      val resolved = rootPath.resolve(path)
      val ext = path.toString().substringAfterLast('.', "")
      when (ext) {
        "kt",
        "kts" -> kotlinTokens += countKotlinTokens(resolved)
        "java" -> javaTokens += countJavaTokens(resolved)
      }
    }
    return TokenCounts(kotlinTokens, javaTokens)
  }

  private val SKIPPED_TOKEN_TYPES =
      setOf(
          KtTokens.WHITE_SPACE,
          KtTokens.EOL_COMMENT,
          KtTokens.BLOCK_COMMENT,
          KtTokens.DOC_COMMENT,
          KtTokens.SHEBANG_COMMENT,
      )

  private fun countKotlinTokens(file: Path): Long {
    val text = Files.readString(file)
    val lexer = KotlinLexer()
    lexer.start(text)
    var count = 0L
    while (lexer.tokenType != null) {
      if (lexer.tokenType !in SKIPPED_TOKEN_TYPES) {
        count++
      }
      lexer.advance()
    }
    return count
  }

  private fun countJavaTokens(file: Path): Long {
    // Use StreamTokenizer for Java - lighter weight, no extra deps
    val text = Files.readString(file)
    val reader = java.io.StreamTokenizer(java.io.StringReader(text))
    reader.slashSlashComments(true)
    reader.slashStarComments(true)
    reader.ordinaryChar('/'.code)
    var count = 0L
    while (reader.nextToken() != java.io.StreamTokenizer.TT_EOF) {
      count++
    }
    return count
  }
}

data class TokenCounts(val kotlinTokens: Long, val javaTokens: Long) {
  val totalTokens: Long
    get() = kotlinTokens + javaTokens
}
