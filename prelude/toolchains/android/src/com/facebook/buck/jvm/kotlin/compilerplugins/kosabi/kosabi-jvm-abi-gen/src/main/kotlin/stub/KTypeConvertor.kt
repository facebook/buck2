/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package main.kotlin.stub

import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KArrayType
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KClassType
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KType
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KTypeVariableName
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KWildcardType
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.Type
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.asKType
import org.jetbrains.kotlin.com.intellij.psi.PsiAnnotation
import org.jetbrains.kotlin.com.intellij.psi.PsiArrayType
import org.jetbrains.kotlin.com.intellij.psi.PsiClassType
import org.jetbrains.kotlin.com.intellij.psi.PsiPrimitiveType
import org.jetbrains.kotlin.com.intellij.psi.PsiType
import org.jetbrains.kotlin.com.intellij.psi.PsiWildcardType
import org.jetbrains.kotlin.com.intellij.psi.impl.source.PsiClassReferenceType
import org.jetbrains.kotlin.name.FqName

private const val NULLABLE_TEXT = "Nullable"
private val PACKAGES_WITH_NON_NULLABLE_ENTRY =
    setOf(
        // for guava ImmutableCollection, entries are default
        // non-nullable
        "com.google.common.collect.Immutable",
    )

@SuppressWarnings("PackageLocationMismatch")
class KTypeConvertor(private val imports: Set<FqName>, private val topLevelClassPackage: String) {

  // put Unit as default for now
  fun parseType(
      psiType: PsiType?,
      annotations: Array<PsiAnnotation>,
      typeVariables: List<KTypeVariableName> = emptyList(),
      boxTypeDefaultNullable: Boolean = true,
  ): Type {
    if (psiType == null) {
      return KType.UNIT
    }

    val isNullable = annotations.any { it.qualifiedName?.contains(NULLABLE_TEXT) ?: false }

    if (psiType is PsiArrayType) {
      val componentType = psiType.componentType
      return if (componentType is PsiPrimitiveType) {
        when (componentType) {
          PsiType.INT -> IntArray::class.asKType()
          PsiType.BOOLEAN -> BooleanArray::class.asKType()
          PsiType.CHAR -> CharArray::class.asKType()
          PsiType.BYTE -> ByteArray::class.asKType()
          PsiType.SHORT -> ShortArray::class.asKType()
          PsiType.LONG -> LongArray::class.asKType()
          PsiType.FLOAT -> FloatArray::class.asKType()
          PsiType.DOUBLE -> DoubleArray::class.asKType()
          else ->
              KArrayType(
                  parseType(componentType, psiType.annotations, typeVariables),
                  nullable = isNullable)
        }
      } else {
        KArrayType(
            parseType(componentType, psiType.annotations, typeVariables), nullable = isNullable)
      }
    }

    if (psiType is PsiPrimitiveType) {
      return parsePsiPrimitiveType(psiType)
    }

    if (psiType is PsiWildcardType) {
      val boundType = psiType.bound
      if (boundType != null) {
        val type = parseType(boundType, boundType.annotations, typeVariables)
        return KWildcardType(
            type,
            if (psiType.isSuper) KWildcardType.BoundType.SUPER else KWildcardType.BoundType.EXTENDS)
      } else {
        return KWildcardType(KType.UNIT, KWildcardType.BoundType.UNBOUNDED)
      }
    }

    if (psiType is PsiClassReferenceType) {
      val boxType = parsePrimitiveBoxType(psiType.className, boxTypeDefaultNullable)
      if (boxType != null) {
        return boxType
      }

      val parseGenericType =
          parseGenericType(psiType as PsiClassType, psiType.annotations, typeVariables)

      val fullQualifiedName = imports.firstOrNull { it.shortName().identifier == psiType.className }
      if (fullQualifiedName != null) {
        return KClassType(
            fullQualifiedName.asString(),
            nullable = isNullable,
            generics = parseGenericType,
        )
      }

      // This will return qualifiedName presenting the psiType
      // For example:
      // java.lang.Class, SomeTypeA.TypeB
      val psiReferenceQualifiedName = psiType.reference.qualifiedName

      // Catch generic type variable name
      val typeVariable = typeVariables.firstOrNull { it.name == psiReferenceQualifiedName }
      if (typeVariable != null) {
        return typeVariable
      }

      // Some case it doesn't have package name and its qualified start with uppercase:
      // Component.Builder<?>
      // We will retrieve the first qualifier to find the package, then form the fullQualifiedName
      // com.facebook. + Component.Builder
      // If we couldn't find the import, we assume it shares the same package with the topLevelClass
      if (psiReferenceQualifiedName.firstOrNull()?.isUpperCase() == true) {
        val qualifier = psiReferenceQualifiedName.split(".").firstOrNull()
        val firstQualifierImportNames =
            imports.firstOrNull { it.shortName().identifier == qualifier }

        val parentPackage: String =
            firstQualifierImportNames?.parent()?.asString() ?: topLevelClassPackage
        return KClassType(
            "$parentPackage.$psiReferenceQualifiedName",
            nullable = isNullable,
            generics = parseGenericType,
        )
      }

      // Here will continue checking built-in java.lang.* PsiClassType that doesn't import
      // Using [psiType.reference.qualifiedName] to avoid wrong fullQualifiedName filling, such as:
      // java.lang.Class<? extends Example> or java.lang.Class<*>
      return KClassType(
          psiReferenceQualifiedName, nullable = isNullable, generics = parseGenericType)
    }

    return KType(psiType.canonicalText, nullable = isNullable)
  }

  private fun parseGenericType(
      classPsi: PsiClassType,
      annotations: Array<PsiAnnotation>,
      typeVariables: List<KTypeVariableName>
  ): List<Type> {
    // here we only check the parameters from the List, Map, collection, etc.
    val classFqName = classPsi.resolve()?.qualifiedName ?: classPsi.className
    val isNonNullEnforceGenericType =
        PACKAGES_WITH_NON_NULLABLE_ENTRY.any { classFqName.startsWith(it) }
    val params = classPsi.parameters

    return if (params.isEmpty()) {
      emptyList()
    } else {
      params.map {
        parseType(
            psiType = it,
            annotations = annotations,
            typeVariables = typeVariables,
            !isNonNullEnforceGenericType)
      }
    }
  }

  private fun parsePsiPrimitiveType(psiType: PsiPrimitiveType): KType {
    return when (psiType) {
      PsiType.BOOLEAN -> KType.BOOLEAN
      PsiType.VOID -> KType.UNIT
      PsiType.BYTE -> KType.BYTE
      PsiType.CHAR -> KType.CHAR
      PsiType.INT -> KType.INT
      PsiType.DOUBLE -> KType.DOUBLE
      PsiType.FLOAT -> KType.FLOAT
      PsiType.LONG -> KType.LONG
      PsiType.SHORT -> KType.SHORT
      else -> KType.UNIT
    }
  }

  // for box type, there is no import for "java.lang.*", use className
  private fun parsePrimitiveBoxType(className: String, nullable: Boolean): KType? {

    return when (Pair(className, nullable)) {
      Pair("Byte", true) -> KType.BYTE_NULLABLE
      Pair("Byte", false) -> KType.BYTE
      Pair("Short", true) -> KType.SHORT_NULLABLE
      Pair("Short", false) -> KType.SHORT
      Pair("Integer", true) -> KType.INT_NULLABLE
      Pair("Integer", false) -> KType.INT
      Pair("Long", true) -> KType.LONG_NULLABLE
      Pair("Long", false) -> KType.LONG
      Pair("Character", true) -> KType.CHAR_NULLABLE
      Pair("Character", false) -> KType.CHAR
      Pair("Float", true) -> KType.FLOAT_NULLABLE
      Pair("Float", false) -> KType.FLOAT
      Pair("Double", true) -> KType.DOUBLE_NULLABLE
      Pair("Double", false) -> KType.DOUBLE
      Pair("Boolean", true) -> KType.BOOLEAN_NULLABLE
      Pair("Boolean", false) -> KType.BOOLEAN
      else -> null
    }
  }
}
