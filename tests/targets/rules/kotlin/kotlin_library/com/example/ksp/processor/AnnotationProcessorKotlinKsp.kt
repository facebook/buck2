package com.example.ksp.processor

import com.example.ksp.annotations.JavaAnnotation
import com.example.ksp.annotations.KspKotlinAnnotation
import com.google.devtools.ksp.processing.CodeGenerator
import com.google.devtools.ksp.processing.Dependencies
import com.google.devtools.ksp.processing.KSPLogger
import com.google.devtools.ksp.processing.Resolver
import com.google.devtools.ksp.processing.SymbolProcessor
import com.google.devtools.ksp.processing.SymbolProcessorEnvironment
import com.google.devtools.ksp.processing.SymbolProcessorProvider
import com.google.devtools.ksp.symbol.KSAnnotated
import com.google.devtools.ksp.symbol.KSClassDeclaration
import com.google.devtools.ksp.symbol.KSVisitorVoid
import com.squareup.kotlinpoet.FileSpec
import com.squareup.kotlinpoet.TypeSpec
import java.io.OutputStreamWriter
import java.io.PrintWriter
import javax.annotation.processing.*

class AnnotationProcessorKotlinKsp(
    private val codeGenerator: CodeGenerator,
    private val logger: KSPLogger,
    private val options: Map<String, String>
) : SymbolProcessor {

  override fun process(resolver: Resolver): List<KSAnnotated> {

    val kotlinAnnotationVisitor: KSVisitorVoid =
        object : KSVisitorVoid() {
          override fun visitClassDeclaration(classDeclaration: KSClassDeclaration, data: Unit) {
            val pkgName = classDeclaration.packageName.asString()
            val name = classDeclaration.simpleName.asString()
            generateKotlinClass(name, pkgName)
          }
        }

    val javaAnnotationVisitor: KSVisitorVoid =
        object : KSVisitorVoid() {
          override fun visitClassDeclaration(classDeclaration: KSClassDeclaration, data: Unit) {
            val pkgName = classDeclaration.packageName.asString()
            val name = classDeclaration.simpleName.asString()
            generateJavaClass(name, pkgName)
          }
        }

    resolver.getSymbolsWithAnnotation(KspKotlinAnnotation::class.qualifiedName!!).forEach {
      it.accept(kotlinAnnotationVisitor, Unit)
    }

    resolver.getSymbolsWithAnnotation(JavaAnnotation::class.qualifiedName!!).forEach {
      it.accept(javaAnnotationVisitor, Unit)
    }

    return emptyList()
  }

  private fun generateKotlinClass(name: String, pkg: String) {
    val fileName = "${name}_kspgen_kotlin_class"

    val fileSpec =
        FileSpec.builder(pkg, fileName).addType(TypeSpec.classBuilder(fileName).build()).build()

    val fileOutputStream = codeGenerator.createNewFile(Dependencies.ALL_FILES, pkg, fileName)
    val writer = OutputStreamWriter(fileOutputStream)
    writer.use(fileSpec::writeTo)
  }

  private fun generateJavaClass(name: String, pkg: String) {
    val fileName = "${name}_kspgen_java_class"

    val fileSpec =
        FileSpec.builder(pkg, fileName).addType(TypeSpec.classBuilder(fileName).build()).build()

    val fileOutputStream =
        codeGenerator.createNewFile(Dependencies.ALL_FILES, pkg, fileName, "java")
    val out = PrintWriter(fileOutputStream)
    out.println("package " + pkg + ";\n")
    out.println("public class " + fileName + " {}")
    out.close()
  }
}

class AnnotationProcessorKotlinKspProvider : SymbolProcessorProvider {
  override fun create(env: SymbolProcessorEnvironment): SymbolProcessor {
    return AnnotationProcessorKotlinKsp(env.codeGenerator, env.logger, env.options)
  }
}
