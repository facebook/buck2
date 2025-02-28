/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.apemulators

import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KFunStub
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KStub
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KType
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.asKType
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.GenerationContext
import com.facebook.kotlin.compilerplugins.kosabi.stubsgen.generators.StubsGenerator
import java.io.IOException
import org.jetbrains.kotlin.psi.KtClass
import org.jetbrains.kotlin.psi.KtClassOrObject
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtVisitorVoid
import org.jetbrains.kotlin.util.isOrdinaryClass

class IgParserStubsGenerator : StubsGenerator {
  override fun generateStubs(context: GenerationContext) {
    val jsonClasses = findJsonClasses(context.projectFiles)
    jsonClasses
        .filter { it.name != null }
        .map { createJsonHelper(it, it.shouldGenerateSerializer()) }
        .forEach { context.stubsContainer.add(it) }
  }

  private fun findJsonClasses(ktFiles: Collection<KtFile>): List<KtClass> =
      ktFiles.flatMap { findClassesAnnotatedByJsonType(it) }

  private fun KtClass?.shouldGenerateSerializer(): Boolean {
    val jsonAnnotationEntry =
        this?.annotationEntries?.find { it.shortName?.identifier == JSON_TYPE_ANNO_SHORT_NAME }
            ?: return true
    val arguments = jsonAnnotationEntry.valueArgumentList?.arguments ?: return true
    return arguments.all {
      it.getArgumentName()?.text != GENERATE_SERIALIZER_ARG ||
          it.getArgumentExpression()?.text != GENERATE_SERIALIZER_NO_VALUE
    }
  }

  private fun createJsonHelper(clazz: KtClass, generateSerializer: Boolean): KStub {
    val pkg = (clazz.containingFile as KtFile).packageFqName.asString()
    val originalClass = KType(clazz.fqName!!.asString())
    // TODO: we should verify this stubs wasn't created before
    /**
     * public final class T__JsonHelper implements com.instagram.common.json.JsonHelper<T> { public
     * static T parseFromJson(com.fasterxml.jackson.core.JsonParser) throws java.io.IOException;
     * public static boolean processSingleField(T, java.lang.String,
     * com.fasterxml.jackson.core.JsonParser) throws java.io.IOException; public static T
     * parseFromJson(java.lang.String) throws java.io.IOException; public static void
     * serializeToJson(com.fasterxml.jackson.core.JsonGenerator,T, boolean) throws
     * java.io.IOException; public static java.lang.String serializeToJson(T) throws
     * java.io.IOException; }
     */
    return KStub(pkg, makeHelperClassName(originalClass.names)).apply {
      // We need T__JsonHelper classes in ABI
      treatAsReal = true
      // Only Objects or Companion objects could have static methods
      type = KStub.Type.OBJECT
      implements = listOf(JSON_HELPER_TYPE.parametriseWith(listOf(originalClass)))

      funStubs =
          mutableListOf(
              KFunStub.withTypedArgs("parseFromJson", listOf(JSON_PARSER_TYPE)).apply {
                static = true
                throws = listOf(IOException::class.asKType())
                ret = originalClass
              },
              KFunStub.withTypedArgs(
                      "processSingleField",
                      listOf(originalClass, String::class.asKType(), JSON_PARSER_TYPE))
                  .apply {
                    static = true
                    throws = listOf(IOException::class.asKType())
                    ret = KType.BOOLEAN
                  },
              KFunStub.withTypedArgs("parseFromJson", listOf(String::class.asKType())).apply {
                static = true
                throws = listOf(IOException::class.asKType())
                ret = originalClass
              })
      if (generateSerializer) {
        funStubs.add(
            KFunStub.withTypedArgs(
                    "serializeToJson", listOf(JSON_GENERATOR_TYPE, originalClass, KType.BOOLEAN))
                .apply {
                  static = true
                  throws = listOf(IOException::class.asKType())
                  ret = KType.UNIT
                })

        funStubs.add(
            KFunStub.withTypedArgs("serializeToJson", listOf(originalClass)).apply {
              static = true
              throws = listOf(IOException::class.asKType())
              ret = String::class.asKType()
            })
      }
    }
  }

  private fun makeHelperClassName(original: List<String>) =
      "${original.joinToString(separator = "_")}__JsonHelper"
}

private const val GENERATE_SERIALIZER_ARG = "generateSerializer"
private const val GENERATE_SERIALIZER_NO_VALUE = "JsonType.TriState.NO"
private const val JSON_TYPE_ANNO_SHORT_NAME = "JsonType"
private val JSON_HELPER_TYPE = KType("com.instagram.common.json.JsonHelper")
private val JSON_PARSER_TYPE = KType("com.fasterxml.jackson.core.JsonParser")
private val JSON_GENERATOR_TYPE = KType("com.fasterxml.jackson.core.JsonGenerator")

fun findClassesAnnotatedByJsonType(ktFile: KtFile): List<KtClass> {
  val classes = mutableListOf<KtClass>()
  val jsonTypeAnnotatedCollectingVisitor =
      object : KtVisitorVoid() {
        override fun visitClassOrObject(classOrObject: KtClassOrObject) {
          if (classOrObject.isOrdinaryClass) {
            if (classOrObject.annotationEntries.any {
              it.shortName?.identifier == JSON_TYPE_ANNO_SHORT_NAME
            })
                classes.add(classOrObject as KtClass)
          }
          classOrObject.declarations.filterIsInstance<KtClass>()?.forEach { it.accept(this) }
          super.visitClassOrObject(classOrObject)
        }
      }
  ktFile.acceptChildren(jsonTypeAnnotatedCollectingVisitor)
  return classes
}
