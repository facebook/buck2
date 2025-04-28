/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.kotlin.compilerplugins.kosabi.common.stub.render

import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KPropertyStub
import com.facebook.kotlin.compilerplugins.kosabi.common.stub.model.KStub
import java.io.File
import java.io.FileOutputStream
import org.jetbrains.org.objectweb.asm.ClassVisitor
import org.jetbrains.org.objectweb.asm.ClassWriter
import org.jetbrains.org.objectweb.asm.Opcodes

object StubBytecodeRender {

  fun KStub.supportBytecode(): Boolean {

    // Kotlin top level functions need extra effort as some kotlin metadata are needed in the class
    // &
    // jar files
    // disable for now until it's supported T219955644
    if (type == KStub.Type.TOP_LEVEL_DECLARATION) {
      return false
    }

    // For KStubs that should be treated as real (e.g JavaKStub for KSP output, or IG JsonHelper
    // type), we don't want to generate bytecode, but should
    // keep
    // passing them as KTFile for compilation, as these classes needed to be part as final ABI
    // output
    if (treatAsReal) {
      return false
    }

    return true
  }

  fun KPropertyStub.modifier(): Int {
    var modifier = Opcodes.ACC_PUBLIC
    if (static) {
      modifier = modifier or Opcodes.ACC_STATIC
    }
    return modifier
  }

  fun KStub.accessFlags(): Int {
    var flag =
        when (type) {
          KStub.Type.ANNOTATION -> Opcodes.ACC_ANNOTATION
          KStub.Type.INTERFACE -> Opcodes.ACC_INTERFACE
          else -> Opcodes.ACC_SUPER
        }

    if (abstract) {
      flag = flag or Opcodes.ACC_ABSTRACT
    }

    flag = flag or Opcodes.ACC_PUBLIC

    return flag
  }

  // The internal name of a class is its FQN,
  // where '.' are replaced by '/') for the package name part, and $ are use to concat the name of
  // inner classes
  // e.g com.foo.bar.Outer.Inner -> com/foo/bar/Outer$Inner
  private fun KStub.internalName(): String {

    var internalName = "$pkg.$name".replace(".", "/")

    var currentSlash = internalName.lastIndexOf('/')
    while (currentSlash > 0) {
      val previousSlash = internalName.lastIndexOf('/', currentSlash - 1)
      internalName =
          if (previousSlash > 0 && Character.isUpperCase(internalName[previousSlash + 1])) {
            ("${internalName.substring(0, currentSlash)}$${internalName.substring(currentSlash + 1)}")
          } else {
            break
          }
      currentSlash = previousSlash
    }
    return internalName
  }

  // Where should we put the class file, respecting the package name hierarchy
  // e.g com/foor/bar/Outer$Inner -> File(root, "com/foo/bar")
  private fun KStub.classFilePath(root: File): File {
    return if (pkg == null) root
    else File(root, internalName().substring(0, internalName().lastIndexOf('/')))
  }

  // The name of the class file output
  // e.g com/foor/bar/Outer$Inner -> "Outer$Inner.class"
  private fun KStub.classFileName(): String {
    return "${internalName().split("/").last()}.class"
  }

  private fun KStub.superClass(): String {
    return "java/lang/Object"
  }

  fun KStub.signature(): String? {
    if (genericTypes == 0) return null
    return "<${(0 until genericTypes).map { "T$it:Ljava/lang/Object;" }.joinToString("")}>"
  }

  private fun KStub.renderInnerClass(visitor: ClassVisitor, dir: File) {
    if (innerStubs.isEmpty()) return

    val containerClassInternalName = internalName()
    innerStubs.forEach { innerStub ->
      renderKStubBytecode(innerStub, dir)
      visitor.visitInnerClass(
          innerStub.internalName(),
          containerClassInternalName,
          innerStub.name,
          innerStub.accessFlags() or Opcodes.ACC_STATIC)
    }
  }

  private fun KStub.renderConstructor(visitor: ClassVisitor) {
    if (ctor == null) return
    visitor.visitMethod(
        Opcodes.ACC_PUBLIC,
        "<init>",
        "(${"Ljava/lang/Object;".repeat(ctor?.namedArgs?.size ?: 0)})V",
        null,
        null)
  }

  fun renderKStubBytecode(stub: KStub?, outputRoot: File) {
    if (stub == null) return

    val classWriter = ClassWriter(ClassWriter.COMPUTE_MAXS)

    classWriter.visit(
        Opcodes.ASM9,
        stub.accessFlags(),
        stub.internalName(),
        stub.signature(),
        stub.superClass(),
        null)

    stub.apply {
      renderInnerClass(classWriter, outputRoot)
      renderConstructor(classWriter)
    }

    classWriter.visitEnd()

    val bytecode = classWriter.toByteArray()

    val folder = stub.classFilePath(outputRoot)
    if (!folder.exists()) folder.mkdirs()

    val classFile = File(folder, stub.classFileName())
    FileOutputStream(classFile).apply {
      write(bytecode)
      close()
    }
  }
}
