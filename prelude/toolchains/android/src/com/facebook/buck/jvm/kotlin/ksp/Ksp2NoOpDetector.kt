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

import com.google.devtools.ksp.processing.CodeGenerator
import com.google.devtools.ksp.processing.Resolver
import com.google.devtools.ksp.processing.SymbolProcessor
import com.google.devtools.ksp.processing.SymbolProcessorProvider
import com.google.devtools.ksp.symbol.KSAnnotated
import java.io.File

/**
 * Counts the files each KSP processor generates, so that a processor attached to a target but
 * generating nothing for it becomes visible.
 *
 * `CodeGenerator.generatedFile` is shared by every processor in an invocation and holds only files
 * whose output streams are still open, which KSP closes at each round boundary. Output is therefore
 * attributable only as a delta taken around each call and summed - exact because KSP runs
 * processors sequentially: `process()` per round, then `finish()` once after the last round.
 *
 * The final-round placeholder `com/facebook/Dummy.java` is not counted. The final-round triggers of
 * the shared KSP-run protocol (`//fbandroid/java/com/facebook/kotlin/ksp/runprotocol`) write it to
 * force a final processing round, precisely when a round generated nothing, so counting it would
 * hide the very no-op this class reports. `KspStepsBuilder` strips the same file from the staged
 * sources.
 *
 * Zero is evidence of a no-op only under non-incremental processing: a processor with nothing to do
 * in an incremental round legitimately generates nothing.
 */
class Ksp2NoOpDetector {

  private val generatedCounts = mutableMapOf<String, Int>()

  /** Files generated per provider class name. A provider whose processor never ran is 0. */
  val countsByProcessor: Map<String, Int>
    get() = generatedCounts.toMap()

  fun wrap(providers: List<SymbolProcessorProvider>): List<SymbolProcessorProvider> =
      providers.map { provider ->
        SymbolProcessorProvider { environment ->
          val processor = provider.create(environment)
          val name = provider::class.java.name
          // Seed the entry so a provider whose processor never generates is distinguishable from
          // one that was never attached. That distinction is the point of this class.
          generatedCounts.putIfAbsent(name, 0)
          Counting(processor, name, environment.codeGenerator)
        }
      }

  private inner class Counting(
      private val delegate: SymbolProcessor,
      private val name: String,
      private val codeGenerator: CodeGenerator,
  ) : SymbolProcessor by delegate {

    override fun process(resolver: Resolver): List<KSAnnotated> = counted {
      delegate.process(resolver)
    }

    override fun finish() {
      counted { delegate.finish() }
    }

    private fun <T> counted(call: () -> T): T {
      val before = generatedCount()
      return call().also {
        generatedCounts[name] = generatedCounts.getValue(name) + (generatedCount() - before)
      }
    }

    private fun generatedCount(): Int =
        codeGenerator.generatedFile.count { !isFinalRoundPlaceholder(it) }
  }

  private companion object {
    /** Relative to KSP's Java output directory; the path `KspStepsBuilder` removes. */
    const val FINAL_ROUND_PLACEHOLDER = "com/facebook/Dummy.java"

    fun isFinalRoundPlaceholder(file: File): Boolean =
        file.invariantSeparatorsPath.endsWith("/$FINAL_ROUND_PLACEHOLDER")
  }
}
