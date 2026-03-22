/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.net.URI
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.config.CompilerConfigurationKey
import org.jetbrains.kotlin.fir.FirElement
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.declarations.FirAnonymousObject
import org.jetbrains.kotlin.fir.declarations.FirClassLikeDeclaration
import org.jetbrains.kotlin.fir.declarations.FirDeclarationOrigin
import org.jetbrains.kotlin.fir.declarations.FirEnumEntry
import org.jetbrains.kotlin.fir.declarations.FirRegularClass
import org.jetbrains.kotlin.fir.declarations.FirTypeAlias
import org.jetbrains.kotlin.fir.declarations.utils.classId
import org.jetbrains.kotlin.fir.declarations.utils.isConst
import org.jetbrains.kotlin.fir.declarations.utils.sourceElement
import org.jetbrains.kotlin.fir.expressions.FirQualifiedAccessExpression
import org.jetbrains.kotlin.fir.expressions.FirResolvedQualifier
import org.jetbrains.kotlin.fir.pipeline.FirResult
import org.jetbrains.kotlin.fir.references.symbol
import org.jetbrains.kotlin.fir.resolve.providers.symbolProvider
import org.jetbrains.kotlin.fir.symbols.SymbolInternals
import org.jetbrains.kotlin.fir.symbols.impl.FirCallableSymbol
import org.jetbrains.kotlin.fir.types.ConeErrorType
import org.jetbrains.kotlin.fir.types.ConeKotlinType
import org.jetbrains.kotlin.fir.types.classId
import org.jetbrains.kotlin.fir.types.coneType
import org.jetbrains.kotlin.fir.types.resolvedType
import org.jetbrains.kotlin.fir.types.upperBoundIfFlexible
import org.jetbrains.kotlin.fir.visitors.FirDefaultVisitorVoid
import org.jetbrains.kotlin.load.kotlin.KotlinJvmBinarySourceElement
import org.jetbrains.kotlin.name.ClassId

private const val JAR_FILE_SEPARATOR = "!/"
private const val STUBSGEN_STUBS_JAR = "stubgen_stubs.jar"

/**
 * Collects classpath class usage from a resolved FIR tree for dep file tracking.
 *
 * This replaces the DependencyTracker compiler plugin for kosabi's inner compilation session, since
 * kosabi creates a separate project environment where the dep-tracker's FIR checker extensions are
 * not registered, and kosabi skips the checker phase entirely.
 *
 * Instead, we walk the already-resolved FIR tree post-analysis and extract class usage from
 * declaration-level type references (supertypes, return types, parameter types, property types,
 * annotations, imports, and const val initializers).
 */
class KosabiClassUsageCollector {

  private val uriUsages: MutableSet<URI> = mutableSetOf()
  private val visitedClasses = mutableSetOf<String>()

  /**
   * Walk the resolved FIR tree and collect all referenced classpath classes. Writes results to the
   * dep-tracker output path if available in the configuration.
   */
  fun collectAndDump(analysisResults: FirResult, configuration: CompilerConfiguration) {
    val outputPath = getDepTrackerOutputPath(configuration) ?: return

    for (output in analysisResults.outputs) {
      val session = output.session
      for (firFile in output.fir) {
        recordImports(firFile, session)
        firFile.accept(ClassUsageVisitor(session))
      }
    }

    dump(outputPath)
  }

  private fun recordImports(
      firFile: org.jetbrains.kotlin.fir.declarations.FirFile,
      session: FirSession,
  ) {
    for (firImport in firFile.imports) {
      val fqName = firImport.importedFqName ?: continue
      if (fqName.isRoot) continue
      val classId = ClassId.topLevel(fqName)
      @OptIn(SymbolInternals::class)
      val symbol = session.symbolProvider.getClassLikeSymbolByClassId(classId) ?: continue
      @OptIn(SymbolInternals::class) val firClass = symbol.fir
      recordClassDeclaration(firClass, session)
    }
  }

  @OptIn(SymbolInternals::class)
  private fun recordType(type: ConeKotlinType, session: FirSession) {
    if (type is ConeErrorType) return

    val classId = type.upperBoundIfFlexible().classId ?: return

    // Skip builtins and local classes
    if (classId.isLocal) return
    val pkg = classId.packageFqName.asString()
    if (pkg.startsWith("kotlin") || pkg.startsWith("java")) return

    val symbol = session.symbolProvider.getClassLikeSymbolByClassId(classId) ?: return
    recordClassDeclaration(symbol.fir, session)
  }

  @OptIn(SymbolInternals::class)
  private fun recordClassDeclaration(firClass: FirClassLikeDeclaration, session: FirSession) {
    when (firClass) {
      is FirRegularClass -> {
        if (firClass.origin is FirDeclarationOrigin.BuiltIns) return
        val className = firClass.symbol.classId.asSingleFqName().asString()
        if (!visitedClasses.add(className)) return
        firClass.sourceElement?.let(::recordSource)
        firClass.superTypeRefs.forEach { recordType(it.coneType, session) }
        firClass.annotations
            .map { it.annotationTypeRef }
            .forEach { recordType(it.coneType, session) }
      }
      is FirAnonymousObject -> {
        firClass.superTypeRefs.forEach { recordType(it.coneType, session) }
      }
      is FirTypeAlias -> {
        recordType(firClass.expandedTypeRef.coneType, session)
      }
      else -> {}
    }
  }

  private fun recordSource(source: org.jetbrains.kotlin.descriptors.SourceElement) {
    when (source) {
      is KotlinJvmBinarySourceElement -> addFile(source.binaryClass.location)
      is org.jetbrains.kotlin.load.kotlin.JvmPackagePartSource ->
          source.knownJvmBinaryClass?.location?.let(::addFile)
      is org.jetbrains.kotlin.load.java.sources.JavaSourceElement -> {
        val javaElement = source.javaElement
        if (
            javaElement is org.jetbrains.kotlin.load.java.structure.impl.VirtualFileBoundJavaClass
        ) {
          javaElement.virtualFile?.path?.let(::addFile)
        }
      }
    }
  }

  private fun addFile(path: String) {
    if (
        path.contains(JAR_FILE_SEPARATOR) &&
            !path.split(JAR_FILE_SEPARATOR)[0].endsWith(STUBSGEN_STUBS_JAR)
    ) {
      uriUsages.add(URI("jar:file://$path"))
    }
  }

  private fun dump(path: String) {
    BufferedWriter(FileWriter(File(path), /* append= */ true)).use { writer ->
      for (uri in uriUsages) {
        writer.appendLine(uri.toString())
      }
    }
  }

  /**
   * Visitor that walks expression trees to find class references. Used for visiting const val
   * initializers and other expression contexts where types need to be tracked.
   */
  private inner class ExpressionUsageVisitor(private val session: FirSession) :
      FirDefaultVisitorVoid() {
    override fun visitElement(element: FirElement) {
      element.acceptChildren(this)
    }

    override fun visitResolvedQualifier(resolvedQualifier: FirResolvedQualifier) {
      val classId = resolvedQualifier.classId
      if (classId != null) {
        @OptIn(SymbolInternals::class)
        val symbol = session.symbolProvider.getClassLikeSymbolByClassId(classId)
        if (symbol != null) {
          @OptIn(SymbolInternals::class) recordClassDeclaration(symbol.fir, session)
        }
      }
      resolvedQualifier.acceptChildren(this)
    }

    override fun visitQualifiedAccessExpression(
        qualifiedAccessExpression: FirQualifiedAccessExpression
    ) {
      // Record the type of the expression itself
      try {
        recordType(qualifiedAccessExpression.resolvedType, session)
      } catch (_: Exception) {
        // resolvedType may throw on unresolved expressions
      }
      // Record the dispatch receiver type (e.g., B.Companion in B.bConst)
      val calleeSymbol = qualifiedAccessExpression.calleeReference.symbol
      if (calleeSymbol is FirCallableSymbol<*>) {
        val dispatchClassId = calleeSymbol.callableId.classId
        if (dispatchClassId != null) {
          @OptIn(SymbolInternals::class)
          val classSymbol = session.symbolProvider.getClassLikeSymbolByClassId(dispatchClassId)
          if (classSymbol != null) {
            @OptIn(SymbolInternals::class) recordClassDeclaration(classSymbol.fir, session)
          }
        }
      }
      qualifiedAccessExpression.acceptChildren(this)
    }
  }

  private inner class ClassUsageVisitor(private val session: FirSession) : FirDefaultVisitorVoid() {
    override fun visitElement(element: FirElement) {
      // Default: do not recurse into expressions/bodies
    }

    override fun visitFile(file: org.jetbrains.kotlin.fir.declarations.FirFile) {
      for (declaration in file.declarations) {
        declaration.accept(this)
      }
    }

    override fun visitRegularClass(regularClass: FirRegularClass) {
      for (superTypeRef in regularClass.superTypeRefs) {
        recordType(superTypeRef.coneType, session)
      }
      for (annotation in regularClass.annotations) {
        recordType(annotation.annotationTypeRef.coneType, session)
      }
      for (declaration in regularClass.declarations) {
        declaration.accept(this)
      }
    }

    override fun visitSimpleFunction(
        simpleFunction: org.jetbrains.kotlin.fir.declarations.FirSimpleFunction
    ) {
      recordType(simpleFunction.returnTypeRef.coneType, session)
      for (valueParameter in simpleFunction.valueParameters) {
        recordType(valueParameter.returnTypeRef.coneType, session)
      }
      for (annotation in simpleFunction.annotations) {
        recordType(annotation.annotationTypeRef.coneType, session)
      }
    }

    override fun visitProperty(property: org.jetbrains.kotlin.fir.declarations.FirProperty) {
      recordType(property.returnTypeRef.coneType, session)
      for (annotation in property.annotations) {
        recordType(annotation.annotationTypeRef.coneType, session)
      }
      // Walk const val initializers — these are part of the ABI (inlined constants)
      // and may reference classes not visible from declaration-level types alone.
      // Example: const val x: Int = B.bConst  references B.Companion
      if (property.isConst) {
        property.initializer?.accept(ExpressionUsageVisitor(session))
      }
    }

    override fun visitConstructor(
        constructor: org.jetbrains.kotlin.fir.declarations.FirConstructor
    ) {
      for (valueParameter in constructor.valueParameters) {
        recordType(valueParameter.returnTypeRef.coneType, session)
      }
      for (annotation in constructor.annotations) {
        recordType(annotation.annotationTypeRef.coneType, session)
      }
    }

    override fun visitEnumEntry(enumEntry: FirEnumEntry) {
      recordType(enumEntry.returnTypeRef.coneType, session)
    }

    override fun visitTypeAlias(typeAlias: FirTypeAlias) {
      recordType(typeAlias.expandedTypeRef.coneType, session)
    }
  }

  companion object {
    /**
     * Gets the dep-tracker output path from the compiler configuration. Uses reflection to access
     * DependencyTrackerCommandLineProcessor.ARG_OUTPUT_PATH since the k2-jvm-abi-gen module does
     * not have a direct dependency on the dep-tracker module.
     */
    @Suppress("SwallowedException")
    private fun getDepTrackerOutputPath(configuration: CompilerConfiguration): String? {
      return try {
        val clpClass =
            Class.forName(
                "com.facebook.kotlin.compilerplugins.usedclasses.cli.DependencyTrackerCommandLineProcessor"
            )
        val companionField = clpClass.getDeclaredField("Companion")
        val companion = companionField.get(null)
        val method = companion.javaClass.getDeclaredMethod("getARG_OUTPUT_PATH")
        @Suppress("UNCHECKED_CAST")
        val key = method.invoke(companion) as CompilerConfigurationKey<String>
        configuration.get(key)
      } catch (_: Exception) {
        null
      }
    }
  }
}
