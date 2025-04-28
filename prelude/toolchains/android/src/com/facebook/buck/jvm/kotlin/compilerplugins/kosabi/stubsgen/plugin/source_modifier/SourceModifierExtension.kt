/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.plugin.source_modifier

import com.facebook.kotlin.compilerplugins.kosabi.common.Logger
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.plugin.model.FakeKtFile
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.plugin.model.generateFakeKtFile
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util.isPrimitiveDataType
import java.io.File
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.extensions.ProcessSourcesBeforeCompilingExtension
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi.KtBinaryExpression
import org.jetbrains.kotlin.psi.KtBinaryExpressionWithTypeRHS
import org.jetbrains.kotlin.psi.KtCallExpression
import org.jetbrains.kotlin.psi.KtCallableDeclaration
import org.jetbrains.kotlin.psi.KtClass
import org.jetbrains.kotlin.psi.KtClassInitializer
import org.jetbrains.kotlin.psi.KtClassOrObject
import org.jetbrains.kotlin.psi.KtDotQualifiedExpression
import org.jetbrains.kotlin.psi.KtExpression
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtLambdaExpression
import org.jetbrains.kotlin.psi.KtNamedFunction
import org.jetbrains.kotlin.psi.KtParameter
import org.jetbrains.kotlin.psi.KtParameterList
import org.jetbrains.kotlin.psi.KtPrimaryConstructor
import org.jetbrains.kotlin.psi.KtProperty
import org.jetbrains.kotlin.psi.KtPropertyDelegate
import org.jetbrains.kotlin.psi.KtSecondaryConstructor
import org.jetbrains.kotlin.psi.KtStringTemplateExpression
import org.jetbrains.kotlin.psi.KtValueArgument
import org.jetbrains.kotlin.psi.KtValueArgumentList
import org.jetbrains.kotlin.psi.KtWhenExpression
import org.jetbrains.kotlin.psi.psiUtil.collectDescendantsOfType
import org.jetbrains.kotlin.psi.psiUtil.endOffset
import org.jetbrains.kotlin.psi.psiUtil.findFunctionByName
import org.jetbrains.kotlin.psi.psiUtil.getChildOfType
import org.jetbrains.kotlin.psi.psiUtil.getChildrenOfType
import org.jetbrains.kotlin.psi.psiUtil.getSuperNames
import org.jetbrains.kotlin.psi.psiUtil.isPrivate
import org.jetbrains.kotlin.psi.psiUtil.startOffset

/**
 * [SourceModifierExtension] goal is to modify Kosabi-unfriendly source code.
 *
 * What we change
 * 1. All private properties (stripping out)
 * - Why?
 * - Kotlin MemberCodegen needs to resolve type of a property to pass the generation stage
 *
 * ```
 *    Unfortunately MemberCodegen has a static flow that doesn't allow us to step in with
 *    configuration changes
 * ```
 * - Both typed and untyped properties could have post processing calls like the example below. We
 *   can not figure out a type of listOf(B.CONST_1, B.CONST_2)
 *
 * ```
 *    val x: List<Int> = listOf(B.CONST_1, B_CONST2).map { it.index }
 * ```
 * 2. Untyped private named functions/methods (stripping out)
 * - Why?
 * - A similar story as with properties, but a different codegenerator.
 * 3. init blocks
 * - Why?
 * - similar to 2
 * 4. Delegation for non-private properties (stripping out the `by ...` part)
 * - Why?
 * - Kotlin MemberCodegen needs to resolve a `Delegate` type in `val x: T by Delegate()` even if
 *   property has an explicit type.
 *
 * Do we break anything with this extension?
 * - Unlikely in 1, 2 and 3
 * - inline functions can't use non-public properties/methods (Kotlin 1.6)
 * - Probably in 4
 * - Delegate stripping might change the way property modifiers process during the codegen as it
 *   changes FieldDescriptor annotatedField. See JB/Kotlin PropertyCodegen::generateBackingField
 *   method
 */
class SourceModifierExtension(
    private val strippedSrcDumpDir: File? = null,
) : ProcessSourcesBeforeCompilingExtension {

  /**
   * [cachedProcessedSources] prevents sources to be stripped multiple times after Kotlin 1.8
   * upgrades.
   *
   * We've noticed [ProcessSourcesBeforeCompilingExtension] behavior is changed and the
   * processSources will be executed more than 1 times during compile process. To avoid from
   * rewrite/remodify sources multiple times, which leads to heavy cost to performance and failure
   * to compilation, we will track if the sources is processed before.
   *
   * For details please read:
   * https://fb.workplace.com/groups/2953911658034434/permalink/6122621594496742/
   */
  private val cachedProcessedSources: MutableMap<String, KtFile> = mutableMapOf()

  // An API for to invoke Stubsgen as a compiler plugin extension
  override fun processSources(
      sources: Collection<KtFile>,
      configuration: CompilerConfiguration
  ): Collection<KtFile> = processSourcesStandalone(configuration, sources)

  // An API for a Standalone Stubsgen
  fun processSourcesStandalone(
      configuration: CompilerConfiguration,
      sources: Collection<KtFile>
  ): Collection<KtFile> {

    val strippedFiles =
        sources.map { file ->
          when (file) {
            // A file produced by [KStub]
            is FakeKtFile -> file
            else -> {
              val cachedFile = cachedProcessedSources[file.virtualFilePath]
              if (cachedFile != null) {
                return@map cachedFile
              }

              val rangesToStrip = calculateRangesToStrip(file).map { it to "" }
              val rangesToReplace = calculateRangesToReplace(file).map { it to "TODO()" }
              val rangesForTypealiasReplace = calculateKnownTypealiasImportsReplace(file)
              // This is temporary solution for Parcelable
              val generateCodeInParcelable = generateFakeParcelableCodegen(file)
              val replacesRanges =
                  (rangesToStrip +
                          rangesToReplace +
                          rangesForTypealiasReplace +
                          generateCodeInParcelable)
                      .sortedByDescending { it.first.first }

              val nonNestedRanges = removeNestedRanges(replacesRanges)
              val strippedContent: String =
                  nonNestedRanges.fold(file.text) { remainingContent, (range, replacement) ->
                    remainingContent.replaceRange(range, replacement)
                  }
              generateFakeKtFile(file.manager, file.virtualFilePath, file.name, strippedContent)
            }
          }
        }

    // smart cast to access elements
    sources as MutableList<KtFile>

    // In-place modification to change the files in the further processing
    for (i in sources.indices) {
      sources[i] = strippedFiles[i]
      cachedProcessedSources[sources[i].virtualFilePath] = strippedFiles[i]
    }

    outputStrippedSrc(sources)

    return sources
  }

  private fun outputStrippedSrc(files: Collection<KtFile>): Unit {
    if (strippedSrcDumpDir != null) {
      Logger.log("writing stripped src to ${strippedSrcDumpDir.absolutePath}")

      val strippedSrcDir = strippedSrcDumpDir
      check(strippedSrcDir.exists() || strippedSrcDir.mkdirs()) {
        "Could not generate directory: $strippedSrcDir"
      }
      files.forEach { file ->
        val filename = file.name
        File(strippedSrcDir, filename).writeText(file.text)
      }
    }
  }

  private fun calculateRangesToReplace(file: KtFile): List<IntRange> {
    // default value in the constructor
    val defaultConstructor = findDefaultValues<KtPrimaryConstructor>(file)
    val constructors = findDefaultValues<KtSecondaryConstructor>(file)

    // default value in the fun
    val defaultInFun = findDefaultValues<KtNamedFunction>(file)

    // Dot Qualified expression and call expression in property and function
    val localPropertiesAndNamedFunctions = findLocalProperty(file) + findLocalNamedFunction(file)
    val expressionInCallableDeclaration =
        localPropertiesAndNamedFunctions.findExpressionToConvertToTODO()

    val enumInitializers = findEnumInitializers(file)

    return (defaultConstructor +
            constructors +
            defaultInFun +
            expressionInCallableDeclaration +
            enumInitializers)
        .map { it.startOffset until it.endOffset }
  }

  // Default values:
  //  - dot expression
  //  - lambdas: () -> T
  private inline fun <reified T : PsiElement> findDefaultValues(file: KtFile): List<KtExpression> {
    return file
        .collectDescendantsOfType<T>()
        .mapNotNull { it.getChildOfType<KtParameterList>() }
        .mapNotNull { it.getChildrenOfType<KtParameter>() }
        .flatMap { it.toList() }
        .mapNotNull { it.defaultValue }
        // Allow literal values: string/int/boolean...
        .filterNot { it.isPrimitiveDataType() }
  }

  // enum initializers
  //   [42, "hello"] are enum initializers in the following code snippet
  //
  // enum class En(val x: Int, val y: String) {
  //   V(42, "hello")
  // }
  private fun findEnumInitializers(file: KtFile): List<KtValueArgument> {
    return file
        .collectDescendantsOfType<KtClass>()
        .filter { it.isEnum() && !it.isPrivate() }
        .mapNotNull { it.body }
        .flatMap { it.enumEntries }
        .mapNotNull { it.initializerList }
        .flatMap { it.initializers }
        .mapNotNull { it.getChildOfType<KtValueArgumentList>() }
        .flatMap { it.arguments }
  }

  private fun findLocalNamedFunction(file: KtFile): List<KtCallableDeclaration> =
      file.collectDescendantsOfType<KtNamedFunction>().filterNot { it.isLocal }

  private fun findLocalProperty(file: KtFile): List<KtCallableDeclaration> =
      file.collectDescendantsOfType<KtProperty>().filterNot { it.isLocal }

  /**
   * This function finds the expression meet the following conditions, convert them to TODO()
   * - KtCallExpression: val item: Type = methodOf()
   * - KtDotQualifiedExpression: val item: Type = SomeSet.of()
   * - KtWhenExpression: val item: Type = when (some) { ... }
   * - KtLambdaExpression: val item: Type = { ... }
   * - KtStringTemplateExpression: val item: Type = "some $abc"
   * - KtBinaryExpression: val item: Type = some.abc
   * - KtBinaryExpressionWithTypeRHS: val item: Type = some.abc as Type
   *
   * Except: CONST property, we will skip it, the constant value will write into ABI.
   *
   * TODO: We may do in reverse like remove last child except some conditions.
   */
  private fun <T : KtCallableDeclaration> List<T>.findExpressionToConvertToTODO():
      List<KtExpression> =
      filterNot { it.typeReference == null }
          .filterNot { it.hasModifier(KtTokens.CONST_KEYWORD) }
          .flatMap {
            listOfNotNull(
                it.getChildOfType<KtBinaryExpression>(),
                it.getChildOfType<KtBinaryExpressionWithTypeRHS>(),
                it.getChildOfType<KtCallExpression>(),
                it.getChildOfType<KtDotQualifiedExpression>(),
                it.getChildOfType<KtLambdaExpression>(),
                it.getChildOfType<KtStringTemplateExpression>(),
                it.getChildOfType<KtWhenExpression>(),
            )
          }

  private fun calculateRangesToStrip(file: KtFile): List<IntRange> {
    val untypedPrivateNameFunctions =
        file.collectDescendantsOfType<KtNamedFunction>().filter {
          it.isPrivate() && it.typeReference == null
        }

    val privateProps =
        file.collectDescendantsOfType<KtProperty>().filter {
          (it.isMember || it.isTopLevel) &&
              it.isPrivate() &&
              // Avoid private const value due to missing info to generate public const
              // private const val A = "A"
              // const val B = A + "B"
              !(it.hasModifier(KtTokens.CONST_KEYWORD) && it.lastChild.isPrimitiveDataType())
        }

    val inits = file.collectDescendantsOfType<KtClassInitializer>()

    // Delegate stripping might change the way property modifiers process during the codegen
    // as it changes FieldDescriptor annotatedField.
    // See JB/Kotlin PropertyCodegen::generateBackingField method
    val delegatesOnTypedNonPrivateProps =
        file
            .collectDescendantsOfType<KtProperty>()
            .filter { !it.isPrivate() && it.isTypedClassProp() }
            .mapNotNull { it.getChildOfType<KtPropertyDelegate>() }
    return (untypedPrivateNameFunctions + privateProps + delegatesOnTypedNonPrivateProps + inits)
        .map { it.startOffset until it.endOffset }
  }
}

private fun KtProperty.isTypedClassProp(): Boolean = isMember && typeReference != null

private fun removeNestedRanges(ranges: List<Pair<IntRange, String>>): List<Pair<IntRange, String>> {
  val result = mutableListOf<Pair<IntRange, String>>()
  ranges.asReversed().forEach {
    if (result.isEmpty()) {
      result += it
    } else {
      val lastRange = result.last().first
      // check if [it] range is nested or not, if not add it
      if (!lastRange.contains(it.first.first) || !lastRange.contains(it.first.last)) {
        result += it
      }
    }
  }

  return result.reversed()
}

// TODO(T169427735): We want to investigate adding infos to KSP classpath
// The issue here is DI will recognize this candidate during KSP processing to generate files.
// Before that, the short term solution is to swap the correct type in the file.
private val KNOWN_TYPEALIAS_MAPS =
    mapOf(
        "com.facebook.kinject.Inject" to "com.facebook.ultralight.Inject",
    )

private fun calculateKnownTypealiasImportsReplace(file: KtFile): List<Pair<IntRange, String>> =
    file.importList
        ?.imports
        ?.mapNotNull { importDirective ->
          val importName = importDirective.importedFqName?.toString() ?: return@mapNotNull null
          val knownTypeAliasImport = KNOWN_TYPEALIAS_MAPS[importName] ?: return@mapNotNull null
          (importDirective.startOffset until importDirective.endOffset) to
              "import $knownTypeAliasImport"
        }
        ?.toList() ?: emptyList()

/**
 * TODO(T167925065): Remove this when we migrate ABI gen to IR backend. This is a temporary hack in
 *   Kotlin 1.9 since the Parcelize codegen has moved to IR stage (We are using old jvm backend and
 *   terminate earlier). The hack is removing the [@Parcelize] annotation from the class and adding
 *   the necessary methods
 */
private fun generateFakeParcelableCodegen(file: KtFile): List<Pair<IntRange, String>> {
  val parcelizeHacks = mutableListOf<Pair<IntRange, String>>()
  file.collectDescendantsOfType<KtClassOrObject>().forEach { classOrObject ->
    val parcelizeAnnotation =
        classOrObject.annotationEntries.firstOrNull {
          it.typeReference?.nameForReceiverLabel() == "Parcelize"
        }
    // Remove @Parcelize to avoid compiler checker
    if (parcelizeAnnotation != null) {
      parcelizeHacks.add(
          (parcelizeAnnotation.startOffset until parcelizeAnnotation.endOffset) to "")
    } else {
      return@forEach
    }
    if (classOrObject.getSuperNames().any { it == "Parcelable" }) {
      var parcelableCodegen: String = ""
      if (classOrObject.body != null) {
        if (classOrObject.findFunctionByName(WRITETOPARCEL) == null) {
          parcelableCodegen += writeToParcelFun + "\n"
        }
        if (classOrObject.findFunctionByName(DESCRIBECONTENTS) == null) {
          parcelableCodegen += describeContentsFun + "\n"
        }
        if (classOrObject.body!!.allCompanionObjects.isEmpty()) {
          parcelableCodegen += classOrObject.creatorCompanionObject()
        }
        parcelableCodegen += "}"
      }

      // return result
      classOrObject.body?.rBrace?.let {
        parcelizeHacks.add((it.startOffset until it.endOffset) to parcelableCodegen)
      }
          ?: run {
            // no body, just add at the end of the file
            val allCodegen =
                " {\n" +
                    writeToParcelFun +
                    "\n" +
                    describeContentsFun +
                    "\n" +
                    classOrObject.creatorCompanionObject() +
                    "\n}"
            val lastChild = classOrObject.lastChild
            parcelizeHacks.add((lastChild.endOffset until lastChild.endOffset) to allCodegen)
          }
    }
  }

  val hasParcelImport =
      file.importDirectives.map { it.importPath?.pathStr }.contains("android.os.Parcel")
  if (parcelizeHacks.isNotEmpty() && !hasParcelImport) {
    // add import android.os.Parcel
    val lastImport = file.importDirectives.last()
    val appendInLastImport =
        """
      |import ${lastImport.importPath?.pathStr}
      |import android.os.Parcel
    """
            .trimMargin()
    parcelizeHacks.add((lastImport.startOffset until lastImport.endOffset) to appendInLastImport)
  }
  return parcelizeHacks
}

private const val CREATOR_NAME = "CREATOR"

private fun KtClassOrObject.creatorCompanionObject() =
    """
  |  companion object $CREATOR_NAME: Parcelable.Creator<$name> {
  |      override fun createFromParcel(parcel: Parcel): $name { return TODO() }
  |      override fun newArray(size: Int): Array<$name?> { return TODO() }
  |  }
"""
        .trimMargin()

private const val WRITETOPARCEL = "writeToParcel"
private val writeToParcelFun =
    """
  |  override final fun $WRITETOPARCEL(parcel: Parcel, flags: Int) {}
"""
        .trimMargin()

private const val DESCRIBECONTENTS = "describeContents"
private val describeContentsFun =
    """
  |  override final fun $DESCRIBECONTENTS(): Int { return TODO() }
"""
        .trimMargin()
