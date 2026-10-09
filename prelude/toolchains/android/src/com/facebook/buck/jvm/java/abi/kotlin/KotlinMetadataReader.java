/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.abi.kotlin;

import com.google.common.collect.ImmutableList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;
import kotlin.Metadata;
import kotlin.metadata.Attributes;
import kotlin.metadata.KmClass;
import kotlin.metadata.KmDeclarationContainer;
import kotlin.metadata.KmProperty;
import kotlin.metadata.Visibility;
import kotlin.metadata.jvm.JvmExtensionsKt;
import kotlin.metadata.jvm.JvmMetadataUtil;
import kotlin.metadata.jvm.JvmMethodSignature;
import kotlin.metadata.jvm.KotlinClassMetadata;
import org.objectweb.asm.tree.AnnotationNode;

/** Utilities to read Kotlin class metadata. */
public class KotlinMetadataReader {

  /**
   * Opaque handle to parsed Kotlin metadata. Callers should obtain this via {@link #readMetadata}
   * and pass it to {@link #getInlineFunctions} and {@link #isFilePrivateClass} to avoid redundant
   * parsing.
   */
  public static final class ParsedMetadata {
    final KotlinClassMetadata metadata;

    ParsedMetadata(KotlinClassMetadata metadata) {
      this.metadata = metadata;
    }
  }

  /**
   * Parses the @kotlin.Metadata annotation into a reusable handle. Call once and pass the result to
   * both {@link #getInlineFunctions} and {@link #isFilePrivateClass}.
   *
   * <p>The strict read only understands versions up to the bundled kotlin-metadata-jvm (e.g. 2.2.0
   * reads up to 2.3.0), so a newer version (e.g. 2.4.0) falls back to a lenient read. Metadata the
   * lenient reader cannot parse fails the action.
   */
  public static ParsedMetadata readMetadata(AnnotationNode annotationNode) {
    Metadata classHeader = createHeader(annotationNode);
    KotlinClassMetadata metadata;
    try {
      metadata = KotlinClassMetadata.readStrict(classHeader);
      if (metadata != null) {
        return new ParsedMetadata(metadata);
      }
    } catch (IllegalArgumentException strictFailure) {
      // Fall through to the lenient read below.
    }
    try {
      metadata = KotlinClassMetadata.readLenient(classHeader);
    } catch (IllegalArgumentException lenientFailure) {
      throw new IllegalArgumentException(
          "Unparsable Kotlin metadata: kind="
              + classHeader.k()
              + " version="
              + Arrays.toString(classHeader.mv())
              + "; strict and lenient reads failed",
          lenientFailure);
    }
    if (metadata == null) {
      throw new IllegalArgumentException(
          "Unparsable Kotlin metadata: kind="
              + classHeader.k()
              + " version="
              + Arrays.toString(classHeader.mv())
              + "; strict and lenient reads failed");
    }
    return new ParsedMetadata(metadata);
  }

  /**
   * Finds the inline functions of a Kotlin class from its parsed metadata.
   *
   * @param parsed the result of {@link #readMetadata}
   */
  public static ImmutableList<String> getInlineFunctions(ParsedMetadata parsed) {
    KotlinClassMetadata metadata = parsed.metadata;
    KmDeclarationContainer container;
    if (metadata instanceof KotlinClassMetadata.Class) {
      container = ((KotlinClassMetadata.Class) metadata).getKmClass();
    } else if (metadata instanceof KotlinClassMetadata.FileFacade) {
      container = ((KotlinClassMetadata.FileFacade) metadata).getKmPackage();
    } else if (metadata instanceof KotlinClassMetadata.MultiFileClassPart) {
      container = ((KotlinClassMetadata.MultiFileClassPart) metadata).getKmPackage();
    } else {
      return ImmutableList.of();
    }

    ImmutableList<String> inlineFunctions =
        container.getFunctions().stream()
            .filter(Attributes::isInline)
            .map(JvmExtensionsKt::getSignature)
            .filter(Objects::nonNull)
            .map(JvmMethodSignature::getName)
            .collect(ImmutableList.toImmutableList());

    ImmutableList<String> inlineProperties =
        container.getProperties().stream()
            .filter(
                it ->
                    Attributes.isInline(it.getGetter())
                        || it.getSetter() != null && Attributes.isInline(it.getSetter()))
            .map(KmProperty::getName)
            .collect(ImmutableList.toImmutableList());

    ImmutableList<String> inlineGetters =
        container.getProperties().stream()
            .filter(it -> Attributes.isInline(it.getGetter()))
            .map(JvmExtensionsKt::getGetterSignature)
            .filter(Objects::nonNull)
            .map(JvmMethodSignature::getName)
            .collect(ImmutableList.toImmutableList());

    ImmutableList<String> inlineSetters =
        container.getProperties().stream()
            .filter(it -> it.getSetter() != null && Attributes.isInline(it.getSetter()))
            .map(JvmExtensionsKt::getSetterSignature)
            .filter(Objects::nonNull)
            .map(JvmMethodSignature::getName)
            .collect(ImmutableList.toImmutableList());

    return Stream.of(inlineFunctions, inlineProperties, inlineGetters, inlineSetters)
        .flatMap(Collection::stream)
        .distinct()
        .sorted()
        .collect(ImmutableList.toImmutableList());
  }

  /**
   * Checks if a Kotlin class is file-private (declared with `private` visibility at the file
   * level). Such classes are compiled to package-private in bytecode, so they pass the standard
   * ACC_PRIVATE check, but they should NOT be included in class-abi since they are not part of the
   * module's public API. Source-only-abi correctly excludes them; this method enables class-abi to
   * match that behavior.
   *
   * @param parsed the result of {@link #readMetadata}
   */
  public static boolean isFilePrivateClass(ParsedMetadata parsed) {
    KotlinClassMetadata metadata = parsed.metadata;
    if (metadata instanceof KotlinClassMetadata.Class) {
      KmClass kmClass = ((KotlinClassMetadata.Class) metadata).getKmClass();
      if (Attributes.getVisibility(kmClass) != Visibility.PRIVATE) {
        return false;
      }
      // Distinguish file-private top-level classes from private inner/nested classes.
      // In Kotlin metadata, inner classes use '.' as a separator (e.g. "com/example/Outer.Inner"),
      // while top-level classes have no '.' after the last '/' (e.g. "com/example/TopLevel").
      String className = kmClass.getName();
      int lastSlash = className.lastIndexOf('/');
      String simpleName = lastSlash >= 0 ? className.substring(lastSlash + 1) : className;
      return !simpleName.contains(".");
    }
    return false;
  }

  /**
   * Converts the given AnnotationNode representing the @kotlin.Metadata annotation into
   * KotlinClassHeader, to be able to use it in KotlinClassMetadata.read.
   */
  private static Metadata createHeader(AnnotationNode node) {
    Integer kind = null;
    int[] metadataVersion = null;
    String[] data1 = null;
    String[] data2 = null;
    String extraString = null;
    String packageName = null;
    Integer extraInt = null;

    Iterator<Object> it = node.values.iterator();
    while (it.hasNext()) {
      String name = (String) it.next();
      Object value = it.next();

      switch (name) {
        case "k":
          kind = (Integer) value;
          break;
        case "mv":
          metadataVersion = listToIntArray(value);
          break;
        case "d1":
          data1 = listToStringArray(value);
          break;
        case "d2":
          data2 = listToStringArray(value);
          break;
        case "xs":
          extraString = (String) value;
          break;
        case "pn":
          packageName = (String) value;
          break;
        case "xi":
          extraInt = (Integer) value;
          break;
      }
    }

    return JvmMetadataUtil.Metadata(
        kind, metadataVersion, data1, data2, extraString, packageName, extraInt);
  }

  @SuppressWarnings("unchecked")
  private static int[] listToIntArray(Object list) {
    return ((List<Integer>) list).stream().mapToInt(i -> i).toArray();
  }

  @SuppressWarnings("unchecked")
  private static String[] listToStringArray(Object list) {
    return ((List<String>) list).toArray(new String[0]);
  }
}
