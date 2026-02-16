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

import kotlin.metadata.Visibility
import kotlin.metadata.jvm.KotlinClassMetadata
import kotlin.metadata.visibility
import org.jetbrains.org.objectweb.asm.AnnotationVisitor
import org.jetbrains.org.objectweb.asm.ClassReader
import org.jetbrains.org.objectweb.asm.ClassVisitor
import org.jetbrains.org.objectweb.asm.ClassWriter
import org.jetbrains.org.objectweb.asm.FieldVisitor
import org.jetbrains.org.objectweb.asm.MethodVisitor
import org.jetbrains.org.objectweb.asm.Opcodes

/** Transforms class file bytecode. Returns null if no transformation was needed. */
interface BytecodeTransformer {
  fun transform(bytes: ByteArray): ByteArray?
}

/**
 * Strips @Throws annotations from bytecode.
 *
 * K2 JVM backend writes @Throws to both the method's throws clause AND to
 * RuntimeInvisibleAnnotations, while K1 only writes the throws clause. Safe Kotlin plugin reads
 *
 * @Throws from RuntimeInvisibleAnnotations and complains even when exceptions are properly caught.
 *   We strip @Throws from annotations while keeping the throws clause intact.
 */
class ThrowsAnnotationStripper : BytecodeTransformer {
  override fun transform(bytes: ByteArray): ByteArray? {
    val reader = ClassReader(bytes)
    val writer = ClassWriter(0) // Don't compute frames/maxs, just copy

    var found = false
    val visitor =
        object : ClassVisitor(Opcodes.ASM9, writer) {
          // Filter class-level annotations
          override fun visitAnnotation(
              descriptor: String,
              visible: Boolean,
          ): AnnotationVisitor? {
            return if (isThrowsAnnotation(descriptor)) {
              found = true
              null // Skip @Throws annotation
            } else {
              super.visitAnnotation(descriptor, visible)
            }
          }

          // Filter method-level annotations
          override fun visitMethod(
              access: Int,
              name: String,
              descriptor: String,
              signature: String?,
              exceptions: Array<out String>?,
          ): MethodVisitor? {
            val methodVisitor = super.visitMethod(access, name, descriptor, signature, exceptions)
            return if (methodVisitor != null) {
              object : MethodVisitor(Opcodes.ASM9, methodVisitor) {
                override fun visitAnnotation(
                    desc: String,
                    visible: Boolean,
                ): AnnotationVisitor? {
                  return if (isThrowsAnnotation(desc)) {
                    found = true
                    null // Skip @Throws annotation
                  } else {
                    super.visitAnnotation(desc, visible)
                  }
                }
              }
            } else {
              null
            }
          }

          // Filter field-level annotations
          override fun visitField(
              access: Int,
              name: String,
              descriptor: String,
              signature: String?,
              value: Any?,
          ): FieldVisitor? {
            val fieldVisitor = super.visitField(access, name, descriptor, signature, value)
            return if (fieldVisitor != null) {
              object : FieldVisitor(Opcodes.ASM9, fieldVisitor) {
                override fun visitAnnotation(
                    desc: String,
                    visible: Boolean,
                ): AnnotationVisitor? {
                  return if (isThrowsAnnotation(desc)) {
                    found = true
                    null // Skip @Throws annotation
                  } else {
                    super.visitAnnotation(desc, visible)
                  }
                }
              }
            } else {
              null
            }
          }

          private fun isThrowsAnnotation(descriptor: String): Boolean {
            return descriptor == "Lkotlin/jvm/Throws;" || descriptor == "Lkotlin/Throws;"
          }
        }

    reader.accept(visitor, 0)
    return if (found) writer.toByteArray() else null
  }
}

/**
 * Strips private declarations from @Metadata annotation in bytecode.
 *
 * Private methods, properties, and constructors persist in Kotlin metadata (serialized into
 *
 * @Metadata annotation's d1/d2 arrays) even after being stripped from IR bytecode. This causes
 *   KSP2/DI processors to see private interface methods and fail with errors.
 *
 * This transformer:
 * 1. Reads the @Metadata annotation from each .class file
 * 2. Parses the metadata using kotlinx-metadata-jvm
 * 3. Filters out private functions, properties, and constructors
 * 4. Writes the modified metadata back to the class file
 */
class PrivateMetadataStripper : BytecodeTransformer {
  private val privateVisibilities =
      setOf(Visibility.PRIVATE, Visibility.PRIVATE_TO_THIS, Visibility.LOCAL)

  override fun transform(bytes: ByteArray): ByteArray? {
    val reader = ClassReader(bytes)

    // First pass: collect metadata annotation values
    var metadataKind: Int? = null
    var metadataVersion: IntArray? = null
    var data1: Array<String>? = null
    var data2: Array<String>? = null
    var extraString: String? = null
    var packageName: String? = null
    var extraInt: Int? = null

    reader.accept(
        object : ClassVisitor(Opcodes.ASM9) {
          override fun visitAnnotation(
              descriptor: String,
              visible: Boolean,
          ): AnnotationVisitor? {
            if (descriptor == "Lkotlin/Metadata;") {
              return object : AnnotationVisitor(Opcodes.ASM9) {
                override fun visit(name: String?, value: Any?) {
                  when (name) {
                    "k" -> metadataKind = value as? Int
                    "mv" -> metadataVersion = value as? IntArray
                    "xs" -> extraString = value as? String
                    "pn" -> packageName = value as? String
                    "xi" -> extraInt = value as? Int
                  }
                }

                override fun visitArray(name: String?): AnnotationVisitor {
                  return object : AnnotationVisitor(Opcodes.ASM9) {
                    private val list = mutableListOf<String>()

                    override fun visit(name: String?, value: Any?) {
                      if (value is String) {
                        list.add(value)
                      }
                    }

                    override fun visitEnd() {
                      when (name) {
                        "d1" -> data1 = list.toTypedArray()
                        "d2" -> data2 = list.toTypedArray()
                      }
                    }
                  }
                }
              }
            }
            return null
          }
        },
        ClassReader.SKIP_CODE or ClassReader.SKIP_DEBUG or ClassReader.SKIP_FRAMES,
    )

    // If no metadata annotation found, skip this file
    if (metadataKind == null || data1 == null || data2 == null) {
      return null
    }

    // Parse the metadata
    val header =
        Metadata(
            kind = metadataKind!!,
            metadataVersion = metadataVersion ?: intArrayOf(),
            data1 = data1!!,
            data2 = data2!!,
            extraString = extraString ?: "",
            packageName = packageName ?: "",
            extraInt = extraInt ?: 0,
        )

    val metadata =
        try {
          KotlinClassMetadata.readStrict(header)
        } catch (e: Exception) {
          return null
        }

    // Filter private declarations based on metadata type
    var modified = false
    when (metadata) {
      is KotlinClassMetadata.Class -> {
        val kmClass = metadata.kmClass
        val functionsRemoved = kmClass.functions.removeAll { it.visibility in privateVisibilities }
        val propertiesRemoved =
            kmClass.properties.removeAll { it.visibility in privateVisibilities }
        val constructorsRemoved =
            kmClass.constructors.removeAll { it.visibility in privateVisibilities }
        // Note: Private supertype stripping is done at the FIR level in
        // stripPrivateSupertypesFromFirMetadataSources() which has access to actual
        // visibility info. The bytecode post-processing approach was too aggressive
        // as it couldn't distinguish between private classes from the same target
        // vs. dependencies from the same package.
        modified = functionsRemoved || propertiesRemoved || constructorsRemoved
      }
      is KotlinClassMetadata.FileFacade -> {
        val kmPackage = metadata.kmPackage
        val functionsRemoved =
            kmPackage.functions.removeAll { it.visibility in privateVisibilities }
        val propertiesRemoved =
            kmPackage.properties.removeAll { it.visibility in privateVisibilities }
        modified = functionsRemoved || propertiesRemoved
      }
      is KotlinClassMetadata.MultiFileClassPart -> {
        val kmPackage = metadata.kmPackage
        val functionsRemoved =
            kmPackage.functions.removeAll { it.visibility in privateVisibilities }
        val propertiesRemoved =
            kmPackage.properties.removeAll { it.visibility in privateVisibilities }
        modified = functionsRemoved || propertiesRemoved
      }
      else -> {
        // SyntheticClass, MultiFileClassFacade, Unknown - no declarations to filter
      }
    }

    // If nothing was modified, skip rewriting the class file
    if (!modified) {
      return null
    }

    // Write the modified metadata back
    val newHeader =
        try {
          metadata.write()
        } catch (e: Exception) {
          return null
        }

    // Second pass: rewrite the class file with updated metadata
    val writer = ClassWriter(0)
    val visitor =
        object : ClassVisitor(Opcodes.ASM9, writer) {
          override fun visitAnnotation(
              descriptor: String,
              visible: Boolean,
          ): AnnotationVisitor? {
            return if (descriptor == "Lkotlin/Metadata;") {
              // Replace with new metadata
              val av = super.visitAnnotation(descriptor, visible)
              av?.visit("k", newHeader.kind)
              av?.visit("mv", newHeader.metadataVersion)
              av?.visitArray("d1")?.apply {
                newHeader.data1.forEach { visit(null, it) }
                visitEnd()
              }
              av?.visitArray("d2")?.apply {
                newHeader.data2.forEach { visit(null, it) }
                visitEnd()
              }
              if (newHeader.extraString.isNotEmpty()) {
                av?.visit("xs", newHeader.extraString)
              }
              if (newHeader.packageName.isNotEmpty()) {
                av?.visit("pn", newHeader.packageName)
              }
              av?.visit("xi", newHeader.extraInt)
              // Return null to skip the original annotation values
              object : AnnotationVisitor(Opcodes.ASM9) {
                override fun visitEnd() {
                  av?.visitEnd()
                }
              }
            } else {
              super.visitAnnotation(descriptor, visible)
            }
          }
        }

    reader.accept(visitor, 0)
    return writer.toByteArray()
  }
}
