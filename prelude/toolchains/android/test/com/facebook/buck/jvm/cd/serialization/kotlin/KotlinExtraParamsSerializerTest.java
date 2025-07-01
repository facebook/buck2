/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.cd.serialization.kotlin;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import com.facebook.buck.core.filesystems.AbsPath;
import com.facebook.buck.jvm.cd.command.kotlin.AnnotationProcessingTool;
import com.facebook.buck.jvm.cd.command.kotlin.KotlinExtraParams;
import com.facebook.buck.jvm.cd.command.kotlin.KotlinSupportedLanguageVersion;
import com.facebook.buck.jvm.cd.serialization.java.ResolvedJavacOptionsSerializer;
import com.facebook.buck.jvm.java.JavacLanguageLevelOptions;
import com.facebook.buck.jvm.java.JavacPluginParams;
import com.facebook.buck.jvm.java.ResolvedJavacOptions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;
import java.io.IOException;
import java.util.Optional;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class KotlinExtraParamsSerializerTest {

  @Rule public TemporaryFolder tmp = new TemporaryFolder();

  @Test
  public void serializeDeserialize() throws IOException {
    ResolvedJavacOptions resolvedJavacOptions =
        new ResolvedJavacOptions(
            Optional.empty(),
            ImmutableList.of(),
            JavacLanguageLevelOptions.DEFAULT,
            false,
            false,
            JavacPluginParams.EMPTY,
            JavacPluginParams.EMPTY,
            ImmutableList.of(),
            null);

    com.facebook.buck.cd.model.java.ResolvedJavacOptions serializedJavacOptions =
        ResolvedJavacOptionsSerializer.serialize(resolvedJavacOptions);

    KotlinExtraParams expected =
        new KotlinExtraParams(
            ImmutableList.of(fsAbsPath("extraClassPath0"), fsAbsPath("extraClassPath1")),
            fsAbsPath("kotlinStdLib"),
            fsAbsPath("annotationProcessingClassPath"),
            AnnotationProcessingTool.JAVAC,
            ImmutableList.of("-version", "-help"),
            ImmutableMap.of(
                fsAbsPath("plugin0"),
                ImmutableMap.of("-foo", "bar"),
                fsAbsPath("plugin1"),
                ImmutableMap.of("-baz", "qux")),
            ImmutableMap.of(
                "plugin2", fsAbsPath("plugin2"),
                "plugin3", fsAbsPath("plugin3")),
            Optional.of("prefix"),
            fsAbsPathSet("friend0", "friend1"),
            ImmutableList.copyOf(fsAbsPathSet("homeLib0", "homeLib1")),
            resolvedJavacOptions,
            Optional.of("8"),
            true,
            Optional.of(fsAbsPath("jvmAbiGen")),
            true,
            Optional.empty(),
            false,
            false,
            false,
            Optional.empty(),
            KotlinSupportedLanguageVersion.V2_0.getValue());

    KotlinExtraParams actual =
        KotlinExtraParamsSerializer.deserialize(
            serializedJavacOptions, KotlinExtraParamsSerializer.serialize(expected));

    assertThat(actual, is(expected));
  }

  private AbsPath fsAbsPath(String name) throws IOException {
    return AbsPath.of(tmp.newFile(name).toPath());
  }

  private ImmutableSortedSet<AbsPath> fsAbsPathSet(String... names) throws IOException {
    ImmutableSortedSet.Builder<AbsPath> builder =
        ImmutableSortedSet.orderedBy(AbsPath.comparator());

    for (String name : names) {
      builder.add(fsAbsPath(name));
    }

    return builder.build();
  }
}
