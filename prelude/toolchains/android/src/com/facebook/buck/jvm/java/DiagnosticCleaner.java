/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;

public final class DiagnosticCleaner {

  private static final Set<String> RECOVERABLE_ERRORS =
      new HashSet<String>() {
        {
          add("compiler.err.cant.resolve.location");
          add("compiler.err.doesnt.exist");
        }
      };

  private DiagnosticCleaner() {}

  public static List<Diagnostic<? extends JavaFileObject>> clean(
      List<Diagnostic<? extends JavaFileObject>> diagnostics) {
    List<Diagnostic<? extends JavaFileObject>> result = removeDuplicates(diagnostics);

    sortRecoverableErrorsLast(result);

    return result;
  }

  private static void sortRecoverableErrorsLast(List<Diagnostic<? extends JavaFileObject>> result) {
    result.sort(
        (o1, o2) -> {
          String code1 = o1.getCode();
          String code2 = o2.getCode();

          if (isRecoverableErrorCode(code1)) {
            if (isRecoverableErrorCode(code2)) {
              return 0;
            }
            return 1;
          } else if (isRecoverableErrorCode(code2)) {
            return -1;
          }

          return 0;
        });
  }

  private static List<Diagnostic<? extends JavaFileObject>> removeDuplicates(
      List<Diagnostic<? extends JavaFileObject>> diagnostics) {
    Set<String> seenDiagnostics = new HashSet<>();
    List<Diagnostic<? extends JavaFileObject>> result = new ArrayList<>(diagnostics.size());

    for (Diagnostic<? extends JavaFileObject> diagnostic : diagnostics) {
      String diagnosticString = diagnosticToString(diagnostic);
      if (!seenDiagnostics.contains(diagnosticString)) {
        result.add(diagnostic);
      }
      seenDiagnostics.add(diagnosticString);
    }
    return result;
  }

  static String diagnosticToString(Diagnostic<? extends JavaFileObject> diagnostic) {
    return String.format(
        "%s %s %d %d %d %d %d %s",
        diagnostic.getKind(),
        diagnostic.getSource(),
        diagnostic.getPosition(),
        diagnostic.getStartPosition(),
        diagnostic.getEndPosition(),
        diagnostic.getLineNumber(),
        diagnostic.getColumnNumber(),
        diagnostic.getCode());
  }

  private static boolean isRecoverableErrorCode(String code) {
    return RECOVERABLE_ERRORS.contains(code);
  }
}
