/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.core.exceptions.HumanReadableException;
import com.facebook.buck.util.string.AsciiBoxStringBuilder;
import com.google.common.base.Throwables;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import javax.annotation.processing.Completion;
import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.Processor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedSourceVersion;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;

/** Wraps an annotation processor, tracing all method calls to BuckEventBus. */
@SupportedSourceVersion(SourceVersion.RELEASE_8)
class TracingProcessorWrapper implements Processor {
  private final Processor innerProcessor;
  private final String buildTargetName;
  private final String annotationProcessorName;

  private int roundNumber = 0;
  private boolean isLastRound = false;

  public TracingProcessorWrapper(String buildTargetName, Processor processor) {
    this.buildTargetName = buildTargetName;
    this.innerProcessor = processor;
    this.annotationProcessorName = innerProcessor.getClass().getName();
  }

  @Override
  public Set<String> getSupportedOptions() {
    try {
      return innerProcessor.getSupportedOptions();
    } catch (RuntimeException | Error e) {
      throw wrapAnnotationProcessorCrashException(e);
    }
  }

  @Override
  public Set<String> getSupportedAnnotationTypes() {
    try {
      return innerProcessor.getSupportedAnnotationTypes();
    } catch (RuntimeException | Error e) {
      throw wrapAnnotationProcessorCrashException(e);
    }
  }

  @Override
  public SourceVersion getSupportedSourceVersion() {
    try {
      return innerProcessor.getSupportedSourceVersion();
    } catch (RuntimeException | Error e) {
      throw wrapAnnotationProcessorCrashException(e);
    }
  }

  @Override
  public void init(ProcessingEnvironment processingEnv) {
    try {
      innerProcessor.init(processingEnv);
    } catch (RuntimeException | Error e) {
      throw wrapAnnotationProcessorCrashException(e);
    }
  }

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    roundNumber += 1;
    isLastRound = roundEnv.processingOver();
    try {
      return innerProcessor.process(annotations, roundEnv);
    } catch (RuntimeException | Error e) {
      throw wrapAnnotationProcessorCrashException(e);
    }
  }

  private HumanReadableException wrapAnnotationProcessorCrashException(Throwable e) {
    List<String> filteredStackTraceLines = getStackTraceEndingAtAnnotationProcessor(e);

    int maxLineLength = filteredStackTraceLines.stream().mapToInt(String::length).max().orElse(75);

    AsciiBoxStringBuilder messageBuilder =
        new AsciiBoxStringBuilder(maxLineLength)
            .writeLine("The annotation processor %s has crashed.\n", annotationProcessorName)
            .writeLine(
                "This is likely a bug in the annotation processor itself, though there may be"
                    + " changes you can make to your code to work around it. Examine the exception"
                    + " stack trace below and consult the annotation processor's troubleshooting"
                    + " guide.\n");

    filteredStackTraceLines.forEach(messageBuilder::writeLine);

    return new HumanReadableException(e, "\n" + messageBuilder);
  }

  private List<String> getStackTraceEndingAtAnnotationProcessor(Throwable e) {
    String[] stackTraceLines = Throwables.getStackTraceAsString(e).split("\n");
    List<String> filteredStackTraceLines = new ArrayList<>();

    boolean skippingBuckFrames = false;
    int numFramesSkipped = 0;
    for (String stackTraceLine : stackTraceLines) {
      if (stackTraceLine.contains(TracingProcessorWrapper.class.getSimpleName())) {
        skippingBuckFrames = true;
        numFramesSkipped = 0;
      } else if (stackTraceLine.contains("Caused by:")) {
        skippingBuckFrames = false;
        if (numFramesSkipped > 0) {
          // Mimic the skipped frames logic of inner exceptions for the frames we've skipped
          filteredStackTraceLines.add(String.format("\t... %d more", numFramesSkipped));
        }
      }

      if (skippingBuckFrames) {
        numFramesSkipped += 1;
        continue;
      }

      filteredStackTraceLines.add(stackTraceLine);
    }
    return filteredStackTraceLines;
  }

  @Override
  public Iterable<? extends Completion> getCompletions(
      Element element, AnnotationMirror annotation, ExecutableElement member, String userText) {
    return innerProcessor.getCompletions(element, annotation, member, userText);
  }
}
