/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi;

import static com.facebook.buck.jvm.java.abi.AbiGenerationModeUtils.usesDependencies;

import com.facebook.buck.cd.model.java.AbiGenerationMode;
import com.facebook.infer.annotation.PropagatesNullable;
import java.util.Objects;
import javax.annotation.Nullable;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.signature.SignatureReader;
import org.objectweb.asm.signature.SignatureWriter;

/**
 * This class fixes up a few details of class ABIs so that they match the way source ABIs generate
 * the same details. It allows us to take potentially risky shortcuts in source ABIs without losing
 * the ability to verify them by binary comparison against class ABIs.
 */
public class SourceAbiCompatibleVisitor extends ClassVisitor {

  private final AbiGenerationMode compatibilityMode;
  @Nullable private String name;

  public SourceAbiCompatibleVisitor(ClassVisitor cv, AbiGenerationMode compatibilityMode) {
    super(Opcodes.ASM9, cv);
    this.compatibilityMode = compatibilityMode;
  }

  @Override
  public void visit(
      int version,
      int access,
      String name,
      String signature,
      String superName,
      String[] interfaces) {
    this.name = name;
    access = stripAbstractFromEnums(access);
    super.visit(version, access, name, fixupSignature(signature), superName, interfaces);
  }

  @Override
  @Nullable
  public MethodVisitor visitMethod(
      int access, String name, String desc, String signature, String[] exceptions) {
    if (!usesDependencies(compatibilityMode) && (access & Opcodes.ACC_BRIDGE) != 0) {
      return null;
    }

    return super.visitMethod(access, name, desc, fixupSignature(signature), exceptions);
  }

  @Override
  public FieldVisitor visitField(
      int access, String name, String desc, String signature, Object value) {
    return super.visitField(access, name, desc, fixupSignature(signature), value);
  }

  @Override
  public void visitInnerClass(String name, String outerName, String innerName, int access) {
    Objects.requireNonNull(this.name);
    if (!usesDependencies(compatibilityMode)
        && !this.name.equals(name)
        && !this.name.equals(outerName)) {
      // Because we can't know the flags for inferred types, InnerClassesTable marks all entries
      // as ACC_STATIC except for the class itself and its member classes. It could technically
      // use the correct flags for non-inferred types, but then it becomes impossible for us to
      // fix up the class ABI to match.
      access = Opcodes.ACC_STATIC;
    }
    access = stripAbstractFromEnums(access);
    super.visitInnerClass(name, outerName, innerName, access);
  }

  private String fixupSignature(@PropagatesNullable String signature) {
    if (signature == null || usesDependencies(compatibilityMode)) {
      return signature;
    }

    SignatureReader reader = new SignatureReader(signature);
    SignatureWriter writer = new SignatureWriter();

    reader.accept(new SourceAbiCompatibleSignatureVisitor(writer));

    return writer.toString();
  }

  private int stripAbstractFromEnums(int access) {
    if (usesDependencies(compatibilityMode)) {
      return access;
    }

    if ((access & Opcodes.ACC_ENUM) == Opcodes.ACC_ENUM) {
      access = access & ~Opcodes.ACC_ABSTRACT;
    }
    return access;
  }
}
