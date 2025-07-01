/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.jvm.java.abi.source;

import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.util.TaskEvent;
import com.sun.source.util.TaskListener;
import java.util.ArrayList;
import java.util.List;

/** Calls back when the ENTER phase is complete. */
abstract class PostEnterCallback implements TaskListener {
  private final List<CompilationUnitTree> compilationUnits = new ArrayList<>();
  private int enterCount = 0;

  @Override
  public void started(TaskEvent e) {
    if (e.getKind() == TaskEvent.Kind.ENTER) {
      enterCount += 1;
    }
  }

  @Override
  public void finished(TaskEvent e) {
    switch (e.getKind()) {
      case PARSE:
        compilationUnits.add(e.getCompilationUnit());
        break;
      case ENTER:
        enterCount -= 1;
        if (enterCount == 0) {
          enterComplete(compilationUnits);
        }
        break;
      // $CASES-OMITTED$
      default:
        break;
    }
  }

  protected abstract void enterComplete(List<CompilationUnitTree> compilationUnits);
}
