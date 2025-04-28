/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.plugin.adapter;

import static org.hamcrest.MatcherAssert.assertThat;

import com.facebook.buck.jvm.java.testutil.compiler.CompilerTreeApiTestRunner;
import com.facebook.buck.jvm.java.testutil.compiler.TestCompiler;
import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableList;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.Filer;
import javax.annotation.processing.RoundEnvironment;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import org.hamcrest.Matchers;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(CompilerTreeApiTestRunner.class)
public class PostEnterTaskListenerTest {
  @Rule public TestCompiler compiler = new TestCompiler();
  private final List<String> callbacksIssued = new ArrayList<>();

  @Test
  public void testCallbackIssuedAfterJustEnterWithoutAPs() throws IOException {
    compiler.addSourceFileContents("Foo.java", "class Foo { }");
    compiler.setProcessors(ImmutableList.of());
    addCallback("callback");
    compiler.enter();

    assertThat(callbacksIssued, Matchers.contains("callback: Foo"));
  }

  @Test
  public void testCallbackIssuedAfterEnterWithoutAPs() throws IOException {
    compiler.addSourceFileContents("Foo.java", "class Foo { }");
    compiler.setProcessors(ImmutableList.of());
    addCallback("callback");
    compiler.compile();

    assertThat(callbacksIssued, Matchers.contains("callback: Foo"));
  }

  @Test
  public void testCallbacksIssuedInOrderRegistered() throws IOException {
    compiler.addSourceFileContents("Foo.java", "class Foo { }");
    compiler.setProcessors(ImmutableList.of());
    addCallback("4");
    addCallback("3");
    addCallback("2");
    addCallback("1");
    compiler.compile();

    assertThat(callbacksIssued, Matchers.contains("4: Foo", "3: Foo", "2: Foo", "1: Foo"));
  }

  @Test
  public void testCallbackIssuedForTopLevelTypesOnly() throws IOException {
    compiler.addSourceFileContents(
        "Foo.java", Joiner.on('\n').join("class Foo {", "  class Inner { }", "}", "class Bar { }"));
    compiler.setProcessors(ImmutableList.of());
    addCallback("callback");
    compiler.compile();

    assertThat(callbacksIssued, Matchers.contains("callback: Foo, Bar"));
  }

  @Test
  public void testCallbackIssuedForPackageIfPackageInfoFilePresent() throws IOException {
    compiler.addSourceFileContents("package-info.java", "package com.example.buck;");
    compiler.setProcessors(ImmutableList.of());
    addCallback("callback");
    compiler.compile();

    assertThat(callbacksIssued, Matchers.contains("callback: buck"));
  }

  @Test
  public void testCallbackIssuedAfterEnterWithAPs() throws IOException {
    // We add an unrelated source file here to make the compiler do something
    compiler.addSourceFileContents("Bar.java", "class Bar { }");
    compiler.setProcessors(
        ImmutableList.of(
            new AbstractProcessor() {
              @Override
              public Set<String> getSupportedAnnotationTypes() {
                return Collections.singleton("*");
              }

              @Override
              public boolean process(
                  Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
                if (roundEnv.processingOver()) {
                  Filer filer = processingEnv.getFiler();
                  // We wait until the last possible moment to generate Foo; if the callback is able
                  // to get an element for it, that means it ran after the enter phase
                  try (OutputStream outputStream =
                      filer.createSourceFile("Foo").openOutputStream()) {
                    outputStream.write("class Foo { }".getBytes());
                  } catch (IOException e) {
                    throw new AssertionError(e);
                  }
                }
                return false;
              }
            }));
    addCallback("callback");
    compiler.compile();

    assertThat(callbacksIssued, Matchers.contains("callback: Bar, Foo"));
  }

  private void addCallback(String name) {
    compiler.addPostEnterCallback(
        topLevelElements ->
            callbacksIssued.add(
                String.format(
                    "%s: %s",
                    name,
                    topLevelElements.stream()
                        .map(Element::getSimpleName)
                        .collect(Collectors.joining(", ")))));
  }
}
