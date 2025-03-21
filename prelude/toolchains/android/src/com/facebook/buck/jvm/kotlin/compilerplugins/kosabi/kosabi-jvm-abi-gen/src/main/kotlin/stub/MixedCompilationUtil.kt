/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.stub

import com.facebook.kotlin.compilerplugins.common.createNewKtFile
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KAnnotation
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KClassType
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KFunStub
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KPropertyStub
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KStub
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KTypeVariableName
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.Type
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.render.render
import java.io.File
import java.net.URL
import main.kotlin.stub.KTypeConvertor
import org.jetbrains.kotlin.cli.jvm.config.javaSourceRoots
import org.jetbrains.kotlin.com.intellij.lang.jvm.JvmModifier
import org.jetbrains.kotlin.com.intellij.openapi.project.Project
import org.jetbrains.kotlin.com.intellij.openapi.util.io.FileUtil
import org.jetbrains.kotlin.com.intellij.openapi.vfs.VfsUtilCore
import org.jetbrains.kotlin.com.intellij.openapi.vfs.VirtualFile
import org.jetbrains.kotlin.com.intellij.openapi.vfs.VirtualFileManager
import org.jetbrains.kotlin.com.intellij.psi.JavaTokenType
import org.jetbrains.kotlin.com.intellij.psi.PsiAnnotation
import org.jetbrains.kotlin.com.intellij.psi.PsiArrayInitializerExpression
import org.jetbrains.kotlin.com.intellij.psi.PsiClass
import org.jetbrains.kotlin.com.intellij.psi.PsiClassType
import org.jetbrains.kotlin.com.intellij.psi.PsiExpression
import org.jetbrains.kotlin.com.intellij.psi.PsiIdentifier
import org.jetbrains.kotlin.com.intellij.psi.PsiJavaFile
import org.jetbrains.kotlin.com.intellij.psi.PsiJavaToken
import org.jetbrains.kotlin.com.intellij.psi.PsiKeyword
import org.jetbrains.kotlin.com.intellij.psi.PsiManager
import org.jetbrains.kotlin.com.intellij.psi.PsiMethod
import org.jetbrains.kotlin.com.intellij.psi.PsiMethodCallExpression
import org.jetbrains.kotlin.com.intellij.psi.PsiModifierListOwner
import org.jetbrains.kotlin.com.intellij.psi.PsiPrimitiveType
import org.jetbrains.kotlin.com.intellij.psi.PsiReference
import org.jetbrains.kotlin.com.intellij.psi.PsiType
import org.jetbrains.kotlin.com.intellij.psi.PsiTypeParameter
import org.jetbrains.kotlin.com.intellij.psi.impl.source.tree.java.PsiNewExpressionImpl
import org.jetbrains.kotlin.config.CompilerConfiguration
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.psiUtil.collectDescendantsOfType

@SuppressWarnings("PackageLocationMismatch")
object MixedCompilationUtil {

  // During the mixed compilation, we normally treat libraries shared same package name with sources
  // as real file.
  // When it treats as real file, it means it will be written into the ABI.
  // This set presents special cases are required to treat as real file.
  private val PART_OF_ABI_PACKAGES_ALLOWLIST: Set<String> = setOf("hilt_aggregated_deps")

  // UL won't work properly if these annotations exist in converted Kotlin file. and Nullable
  // annotations doesn't make sense in Kotlin so we ignore these as well
  private val SET_OF_IGNORED_ANNOTATIONS: Set<String> =
      setOf("Inject", "Generated", "ExcusesForDesignViolations", "Nullable", "NotNull", "Nullsafe")

  // PSI alone doesn't tell us the right type of an annotation param,
  // In Java you could write { components = "dagger.hilt.android.components.FragmentComponent" }
  // where components is indeed array of String, it's valid because vararg array
  // From PSI we got RHS is JvmAnnotationConstantValue
  // So create this map, where the key is an annotation, and values are annotation param names that
  // the type is array
  private val ANNOTATION_WITH_ARRAY_ARGUMENT_MAP: Map<String, List<String>> =
      mapOf(
          "AggregatedDeps" to
              listOf("components", "replaces", "modules", "entryPoints", "componentEntryPoints"),
      )

  fun convertGeneratedJavaSourcesToKotlin(
      project: Project,
      configuration: CompilerConfiguration,
      files: Collection<KtFile>
  ): List<KtFile> {
    val javaSourceUrls =
        configuration.javaSourceRoots
            .filter { it.contains("ksp_generated_java") || it.endsWith(".java") }
            .mapNotNull {
              VirtualFileManager.constructUrl("file", FileUtil.toSystemIndependentName(it))
            }
    val virtualFileManager = VirtualFileManager.getInstance()
    val psiManager = PsiManager.getInstance(project)
    val localSourcePackageName = files.map { it.packageFqName }.toSet()

    return recursivelyReadAndConvertJavaFromUrls(
        javaSourceUrls, psiManager, virtualFileManager, localSourcePackageName)
  }

  private fun recursivelyReadAndConvertJavaFromUrls(
      urls: List<String>,
      psiManager: PsiManager,
      virtualFileManager: VirtualFileManager,
      localSourcesPackages: Set<FqName>
  ) =
      urls.flatMap { url ->
        val javaSourceVirtualFile =
            virtualFileManager.findFileByUrl(url) ?: return@flatMap emptyList<KtFile>()
        val directory =
            getDirectoryPathFromUrl(javaSourceVirtualFile.url) ?: return@flatMap emptyList<KtFile>()

        val javaSourceFiles = mutableListOf<VirtualFile>()
        if (javaSourceVirtualFile.isDirectory) {
          VfsUtilCore.processFilesRecursively(javaSourceVirtualFile) { file ->
            if (!file.isDirectory) {
              javaSourceFiles.add(file)
            }
            true
          }
        } else {
          javaSourceFiles.add(javaSourceVirtualFile)
        }

        javaSourceFiles.mapNotNull {
          generateKotlinStubFromJavaSource(psiManager, it, directory, localSourcesPackages)
        }
      }

  @Suppress("detekt.SwallowedException")
  private fun getDirectoryPathFromUrl(urlString: String): String? {
    return try {
      val url = URL(urlString)
      val filePath = url.toURI().path
      val file = File(filePath)
      file.parent
    } catch (e: RuntimeException) {
      null
    }
  }

  private fun generateKotlinStubFromJavaSource(
      psiManager: PsiManager,
      javaVirtualFile: VirtualFile,
      writeToDirectoryPath: String,
      localSourcesPackages: Set<FqName>
  ): KtFile? {
    val psiFile = psiManager.findFile(javaVirtualFile) as? PsiJavaFile ?: return null
    val javaToKStubConvertor = JavaToKStubConvertor(psiFile)
    val stub =
        javaToKStubConvertor.stub?.apply {
          // To determine the stubs is dependent or real stub we want to write into package.
          val pkgFqName = FqName(javaToKStubConvertor.pkg)
          treatAsReal =
              localSourcesPackages.contains(pkgFqName) ||
                  PART_OF_ABI_PACKAGES_ALLOWLIST.any { pkg -> pkgFqName.toString().contains(pkg) }
        } ?: return null

    return createNewKtFile(
        psiManager, writeToDirectoryPath, "${stub.name}.kt", stub.render(), writable = true)
  }

  // Converts Java into Kotlin
  private class JavaToKStubConvertor(private val file: PsiJavaFile) {
    val imports: Set<FqName> =
        file.importList
            ?.importStatements
            ?.mapNotNull { it.qualifiedName }
            ?.map { FqName(it) }
            ?.toSet() ?: emptySet()
    val pkg = file.packageName
    val stub: KStub?

    private val topLevelClass: PsiClass? = file.classes.firstOrNull()
    private val typeConvertor: KTypeConvertor = KTypeConvertor(imports, pkg)

    init {
      stub = topLevelClass?.toStub()
    }

    private fun PsiClass.toStub(): KStub? =
        qualifiedName
            ?.let { FqName(it) }
            ?.let { fqName ->
              val pkg = fqName.parent()
              val className = fqName.shortName()
              val stub = KStub(pkg.asString(), className.identifier)
              stub.stubImports =
                  imports
                      .filterNot { it -> it.shortName().identifier in SET_OF_IGNORED_ANNOTATIONS }
                      .map { import -> import.asString() }
              stub.type = getStubType()
              stub.abstract = hasModifier(JvmModifier.ABSTRACT)
              stub.stubAnnotations = getKAnnotations()
              stub.doNotRenderAsKotlinObject =
                  hasModifier(JvmModifier.STATIC) &&
                      stub.stubAnnotations.any { it -> it.name == "dagger.Module" }

              stub.typeVariableList =
                  this.typeParameters.map {
                    KTypeVariableName(it.name ?: "T", it.parseBoundsTypes())
                  }
              stub.extends = parseExtends(this, stub.typeVariableList)
              stub.implements = parseImplements(this, stub.typeVariableList)
              stub.propertyStubs.addAll(parseProperties(this, stub.typeVariableList))
              // check the methods
              stub.funStubs.addAll(parseFunctions(this, stub.typeVariableList))
              stub.innerStubs.addAll(parseInnerClass(this))
              stub
            } ?: run { null }

    private fun PsiClass.getStubType(): KStub.Type =
        when {
          isAnnotationType -> KStub.Type.ANNOTATION
          isInterface -> KStub.Type.INTERFACE
          else -> KStub.Type.CLASS
        }

    private fun PsiModifierListOwner.getKAnnotations(): List<KAnnotation> {
      return annotations
          .filterNot { annotation ->
            SET_OF_IGNORED_ANNOTATIONS.contains(annotation.qualifiedName?.substringAfterLast("."))
          } // Do not retain UL annotation
          .map { annotation ->
            KAnnotation(
                imports
                    .firstOrNull { it.shortName().toString() == annotation.qualifiedName }
                    ?.parent()
                    ?.toString() ?: "",
                annotation.qualifiedName ?: "",
                annotation.propertyText())
          }
    }

    private fun PsiAnnotation.propertyText(): String {
      if (this.parameterList.attributes.isEmpty()) {
        return ""
      }

      val arrayParamNames =
          ANNOTATION_WITH_ARRAY_ARGUMENT_MAP.getOrDefault(
              this.qualifiedName?.substringAfterLast("."), emptyList())

      var annotationText = text
      // Find any vararg string array text and replace with valid kotlin format
      arrayParamNames.forEach {
        annotationText =
            annotationText.replace(Regex("$it\\s*=\\s*\"([^\"]+)\""), "$it = [\"\$1\"]")
      }

      return annotationText
          .substring(annotationText.indexOf("(") + 1, annotationText.length - 1)
          .replace("{", "[")
          .replace("}", "]")
          .replace(".class", "::class")
    }

    private fun parseFunctions(
        psiClass: PsiClass,
        typeVariables: List<KTypeVariableName>
    ): List<KFunStub> {
      return psiClass.methods
          .filterNot { it.shouldSkipTranslation() }
          .filterNot { it.hasModifier(JvmModifier.PRIVATE) && !it.isConstructor }
          .map { method ->
            val typeParameters =
                method.typeParameters.map {
                  KTypeVariableName(it.name ?: "T", it.parseBoundsTypes())
                }
            val kFun =
                if (method.hasParameters()) {
                  KFunStub(
                      method.name,
                      method.parameterList.parameters.map { psiParameter ->
                        Pair(
                            psiParameter.name,
                            typeConvertor.parseType(
                                psiParameter.type,
                                psiParameter.annotations,
                                typeParameters + typeVariables))
                      })
                } else {
                  KFunStub(method.name, method.parameters.size)
                }
            // parse the static modifier
            kFun.static = method.hasModifier(JvmModifier.STATIC)
            kFun.private = method.hasModifier(JvmModifier.PRIVATE)
            kFun.abstract = method.hasModifier(JvmModifier.ABSTRACT)
            kFun.isFinal = method.hasModifier(JvmModifier.FINAL)
            kFun.isConstructor = method.isConstructor
            kFun.typeVariableList = typeParameters
            kFun.annotations = method.getKAnnotations()
            // add operator if name matches the keyword get
            kFun.isOperator = OPERATOR_FUNCTION_KEYWORDS.contains(method.name)
            kFun.apply {
              if (isConstructor && method.returnType == null) {
                // Convert constructor body super statement to super delegation call in return type
                val superDelegationCallExpression =
                    method.body?.collectDescendantsOfType<PsiMethodCallExpression>()?.firstOrNull()
                if (superDelegationCallExpression != null) {
                  val isSuper =
                      superDelegationCallExpression.methodExpression.canonicalText ==
                          PsiKeyword.SUPER
                  val parameterValues =
                      superDelegationCallExpression.argumentList.expressions.map {
                        Pair(it.text, typeConvertor.parseType(it.type, emptyArray()))
                      }
                  constructorSuperDelegate = Pair(isSuper, parameterValues)
                }
              } else {
                ret =
                    typeConvertor.parseType(
                        method.returnType, method.annotations, typeParameters + typeVariables)
              }
            }
          }
    }

    private fun PsiTypeParameter.parseBoundsTypes(): List<Type> =
        (extendsListTypes + implementsListTypes).map { typeConvertor.parseType(it, it.annotations) }

    private fun parseInnerClass(psiClass: PsiClass): List<KStub> {
      // DO NOT USE allInnerClasses
      // TODO: Add test for not using allInnerClasses: T189515204
      return psiClass.innerClasses.mapNotNull { it.toStub() }
    }

    private fun parseExtends(psiClass: PsiClass, typeVariables: List<KTypeVariableName>): Type? {
      val extends = psiClass.extendsListTypes.firstOrNull { it.name != "Annotation" } ?: return null
      return typeConvertor.parseType(extends, extends.annotations, typeVariables)
    }

    private fun parseImplements(
        psiClass: PsiClass,
        typeVariables: List<KTypeVariableName>
    ): List<Type> {
      return psiClass.implementsListTypes.map {
        typeConvertor.parseType(it, it.annotations, typeVariables)
      }
    }

    private fun parseProperties(
        psiClass: PsiClass,
        typeVariables: List<KTypeVariableName>
    ): List<KPropertyStub> {
      return psiClass.fields
          .filter { it.hasModifier(JvmModifier.STATIC) && it.hasModifier(JvmModifier.PUBLIC) }
          .map { field ->
            val stub =
                when (field.initializer) {
                  is PsiReference -> {
                    val typeList = mutableListOf<Type>()
                    val value =
                        field.initializer?.children?.mapNotNull { element ->
                          when (element) {
                            is PsiReference -> {
                              val classFqName = FqName(element.canonicalText)
                              typeList.add(KClassType(classFqName.asString()))
                              "%T"
                            }
                            is PsiIdentifier -> element.text
                            is PsiJavaToken ->
                                if (element.tokenType == JavaTokenType.DOT) "." else null
                            else -> null
                          }
                        }

                    KPropertyStub(field.name, value?.joinToString(""), typeList)
                  }
                  is PsiNewExpressionImpl -> {
                    if ((field.initializer as PsiNewExpressionImpl).isArrayCreation) {
                      KPropertyStub(field.name, parseArrayInitialization(field.initializer))
                    } else {
                      KPropertyStub(field.name, field.initializer?.text)
                    }
                  }
                  else -> KPropertyStub(field.name, field.initializer?.text)
                }
            stub.static = true
            stub.apply {
              ret = typeConvertor.parseType(field.type, field.annotations, typeVariables)
            }
          }
    }
  }
}

private fun parseArrayInitialization(exp: PsiExpression?): String? {
  return when (exp) {
    is PsiNewExpressionImpl -> {
      var arrayInitializer: String? = null
      if ((exp.arrayInitializer?.type?.arrayDimensions ?: 0) > 1) {
        arrayInitializer = "arrayOf"
      } else {
        arrayInitializer =
            when (exp.arrayInitializer?.type?.deepComponentType) {
              PsiType.BYTE -> "byteArrayOf"
              PsiType.BOOLEAN -> "booleanArrayOf"
              PsiType.CHAR -> "charArrayOf"
              PsiType.DOUBLE -> "doubleArrayOf"
              PsiType.FLOAT -> "floatArrayOf"
              PsiType.INT -> "intArrayOf"
              PsiType.SHORT -> "shortArrayOf"
              else -> "arrayOf"
            }
      }

      arrayInitializer + "(" + parseArrayInitialization(exp.arrayInitializer) + ")"
    }
    is PsiArrayInitializerExpression -> {
      exp.initializers
          .map { parseArrayInitialization(it) }
          .joinToString(separator = ",", limit = exp.initializers.size)
    }
    else -> exp?.text
  }
}

private val OPERATOR_FUNCTION_KEYWORDS: Set<String> =
    setOf(KtTokens.GET_KEYWORD.value, KtTokens.SET_KEYWORD.value)

/**
 * We skip translating unused methods in Ultralight DI dynamic injection below:
 *
 *  ```
 *   public int dynamicId(com.google.inject.Key Key)
 *   public com.google.inject.Key intToKey(int id)
 *  ```
 */
private fun PsiMethod.shouldSkipTranslation(): Boolean =
    isUltralightDynamicIDMethod() || isUltralightDynamicIntToKeyMethod()

// public int dynamicId(com.google.inject.Key Key)
private fun PsiMethod.isUltralightDynamicIDMethod(): Boolean =
    name == ULTRALIGHT_FUN_DYNAMIC_ID &&
        parameterList.parameters.any { param -> param.type.isGoogleInjectKeyType() } &&
        returnType is PsiPrimitiveType

// public com.google.inject.Key intToKey(int id)
private fun PsiMethod.isUltralightDynamicIntToKeyMethod() =
    name == ULTRALIGHT_FUN_DYNAMIC_INT_TO_KEY && (returnType?.isGoogleInjectKeyType() ?: false)

private const val ULTRALIGHT_FUN_DYNAMIC_ID = "dynamicId"
private const val ULTRALIGHT_FUN_DYNAMIC_INT_TO_KEY = "intToKey"
private const val ULTRALIGHT_DYNAMIC_GOOGLE_KEY_TYPE = "com.google.inject.Key"

private fun PsiType.isGoogleInjectKeyType(): Boolean =
    this is PsiClassType && this.rawType().canonicalText == ULTRALIGHT_DYNAMIC_GOOGLE_KEY_TYPE
