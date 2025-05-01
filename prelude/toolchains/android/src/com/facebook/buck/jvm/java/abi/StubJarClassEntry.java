/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi;

import com.facebook.buck.cd.model.java.AbiGenerationMode;
import com.facebook.buck.jvm.java.abi.kotlin.InlineFunctionScope;
import com.facebook.buck.jvm.java.abi.kotlin.KotlinMetadataReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import javax.annotation.Nullable;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.AnnotationNode;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.InnerClassNode;

class StubJarClassEntry extends StubJarEntry {
  @Nullable private final Set<String> referencedClassNames;
  private final Path path;
  private final ClassNode stub;
  private final List<String> kotlinInlineFunctions;
  private final boolean isWithinInlineFunctionScope;
  private final boolean keepSynthetic;

  @Nullable
  public static StubJarClassEntry of(
      LibraryReader input,
      Path path,
      @Nullable AbiGenerationMode compatibilityMode,
      @Nullable InlineFunctionScope inlineFunctionScope,
      boolean keepSynthetic)
      throws IOException {

    List<String> inlineFunctions = Collections.emptyList();
    boolean isKotlinModule = inlineFunctionScope != null;
    boolean isKotlinClass = false;
    boolean isWithinInlineFunctionScope = false;

    if (isKotlinModule) {
      // Visit the class (skipping code) without filtering, to gather its annotations and class
      // relationships
      ClassNode classMetadata = new ClassNode(Opcodes.ASM9);
      input.visitClass(path, classMetadata, true);

      AnnotationNode kotlinMetadataAnnotation = findKotlinMetadataAnnotation(classMetadata);
      if (kotlinMetadataAnnotation != null) {
        isKotlinClass = true;
        isWithinInlineFunctionScope = inlineFunctionScope.captures(path, classMetadata);
        inlineFunctions = KotlinMetadataReader.getInlineFunctions(kotlinMetadataAnnotation);
      }
    }

    // As we read the class in, we create a partial stub that removes non-ABI methods and fields
    // but leaves the entire InnerClasses table. We record all classes that are referenced from
    // ABI methods and fields, and will use that information later to filter the InnerClasses table.
    //
    // If this class is within a Kotlin inline function's scope its ABI is unfiltered, because it
    // will be copied in its entirety in the calling context.
    ClassNode stub = new ClassNode(Opcodes.ASM9);
    ClassReferenceTracker referenceTracker = new ClassReferenceTracker(stub);
    ClassVisitor visitor = referenceTracker;
    if (!isWithinInlineFunctionScope) {
      visitor = new AbiFilteringClassVisitor(visitor, inlineFunctions, keepSynthetic);
    }

    // If we want ABIs that are compatible with those generated from source, we add a visitor
    // at the very start of the chain which transforms the event stream coming out of `ClassNode`
    // to look like what ClassVisitorDriverFromElement would have produced.
    if (compatibilityMode != null && compatibilityMode != AbiGenerationMode.CLASS) {
      visitor = new SourceAbiCompatibleVisitor(visitor, compatibilityMode);
    }

    // Don't skip the code if this is a Kotlin Class so that (a) we have a chance to visit the the
    // classes referenced in inline function bodies, and (b) to retain debug information that
    // Kotlin's
    // own jvm-abi-gen plugin would retain.
    input.visitClass(path, visitor, /* skipCode */ !isKotlinClass);

    // Kotlin top functions reside in synthetic classes, we should output ABIs for them.
    if ((isSyntheticClass(stub) && isKotlinModule)
        || !(isSyntheticClass(stub) || isAnonymousOrLocalClass(stub))
        || isWithinInlineFunctionScope
        // The synthetic package-info class is how package annotations are recorded; that one is
        // actually used by the compiler
        || stub.name.endsWith("/package-info")) {
      return new StubJarClassEntry(
          path,
          stub,
          referenceTracker.getReferencedClassNames(),
          inlineFunctions,
          isWithinInlineFunctionScope,
          keepSynthetic);
    }

    return null;
  }

  private StubJarClassEntry(
      Path path,
      ClassNode stub,
      Set<String> referencedClassNames,
      List<String> kotlinInlineFunctions,
      boolean isWithinInlineFunctionScope,
      boolean keepSynthetic) {
    this.path = path;
    this.stub = stub;
    this.referencedClassNames = referencedClassNames;
    this.kotlinInlineFunctions = kotlinInlineFunctions;
    this.isWithinInlineFunctionScope = isWithinInlineFunctionScope;
    this.keepSynthetic = keepSynthetic;
  }

  @Override
  public void write(StubJarWriter writer) {
    writer.writeEntry(path, this::openInputStream);
  }

  @Override
  public List<String> getInlineFunctions() {
    return kotlinInlineFunctions;
  }

  @Override
  public boolean extendsInlineFunctionScope() {
    return isWithinInlineFunctionScope;
  }

  private InputStream openInputStream() {
    ClassWriter writer = new ClassWriter(0);
    ClassVisitor visitor = writer;
    if (!isWithinInlineFunctionScope) {
      visitor = new InnerClassSortingClassVisitor(stub.name, visitor);
      visitor =
          new AbiFilteringClassVisitor(
              visitor, kotlinInlineFunctions, referencedClassNames, keepSynthetic);
    }

    stub.accept(visitor);

    return new ByteArrayInputStream(writer.toByteArray());
  }

  private static boolean isSyntheticClass(ClassNode node) {
    return ((node.access & Opcodes.ACC_SYNTHETIC) == Opcodes.ACC_SYNTHETIC);
  }

  private static boolean isAnonymousOrLocalClass(ClassNode node) {
    InnerClassNode innerClass = getInnerClassMetadata(node);
    while (innerClass != null) {
      if (innerClass.outerName == null) {
        return true;
      }
      innerClass = getInnerClassMetadata(node, innerClass.outerName);
    }

    return false;
  }

  @Nullable
  private static InnerClassNode getInnerClassMetadata(ClassNode node) {
    String name = node.name;
    return getInnerClassMetadata(node, name);
  }

  @Nullable
  private static AnnotationNode findKotlinMetadataAnnotation(ClassNode node) {
    if (node.visibleAnnotations == null) {
      return null;
    }
    return node.visibleAnnotations.stream()
        .filter(annotation -> "Lkotlin/Metadata;".equals(annotation.desc))
        .findFirst()
        .orElse(null);
  }

  @Nullable
  private static InnerClassNode getInnerClassMetadata(ClassNode node, String className) {
    for (InnerClassNode innerClass : node.innerClasses) {
      if (innerClass.name.equals(className)) {
        return innerClass;
      }
    }

    return null;
  }

  private static class InnerClassSortingClassVisitor extends ClassVisitor {
    private final String className;
    private final List<InnerClassNode> innerClasses = new ArrayList<>();
    private final List<String> nestMembers = new ArrayList<>();

    private InnerClassSortingClassVisitor(String className, ClassVisitor cv) {
      super(Opcodes.ASM9, cv);
      this.className = className;
    }

    @Override
    public void visitInnerClass(String name, String outerName, String innerName, int access) {
      innerClasses.add(new InnerClassNode(name, outerName, innerName, access));
    }

    @Override
    public void visitNestMember(String nestMember) {
      nestMembers.add(nestMember);
    }

    @Override
    public void visitEnd() {
      innerClasses.sort(
          (o1, o2) -> {
            // Enclosing classes and member classes should come first, with their order preserved
            boolean o1IsEnclosingOrMember = isEnclosingOrMember(o1);
            boolean o2IsEnclosingOrMember = isEnclosingOrMember(o2);
            if (o1IsEnclosingOrMember && o2IsEnclosingOrMember) {
              // Preserve order among these
              return 0;
            } else if (o1IsEnclosingOrMember) {
              return -1;
            } else if (o2IsEnclosingOrMember) {
              return 1;
            }

            // References to other classes get sorted.
            return o1.name.compareTo(o2.name);
          });

      for (InnerClassNode innerClass : innerClasses) {
        innerClass.accept(cv);
      }

      nestMembers.stream().sorted().forEach(nestMember -> cv.visitNestMember(nestMember));

      super.visitEnd();
    }

    private boolean isEnclosingOrMember(InnerClassNode innerClass) {
      if (className.equals(innerClass.name)) {
        // Self!
        return true;
      }

      if (className.equals(innerClass.outerName)) {
        // Member class
        return true;
      }

      // Enclosing class
      return className.startsWith(innerClass.name + "$");
    }
  }
}
