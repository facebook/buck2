/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.google.common.io.Files;
import difflib.DiffUtils;
import difflib.Patch;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;

public final class JarDiffer {
  private final Path left;
  private final Path right;
  private final JarDumper jarDumper = new JarDumper();

  public static List<String> diffJars(Path left, Path right) throws IOException {
    return new JarDiffer(left, right).diff();
  }

  public JarDiffer(Path left, Path right) {
    this.left = left;
    this.right = right;
  }

  public JarDiffer setAsmFlags(int asmFlags) {
    jarDumper.setAsmFlags(asmFlags);
    return this;
  }

  public List<String> diff() throws IOException {
    if (Files.equal(left.toFile(), right.toFile())) {
      return Collections.emptyList();
    }

    List<String> leftDump = jarDumper.dump(left);
    List<String> rightDump = jarDumper.dump(right);

    Patch<String> diff = DiffUtils.diff(leftDump, rightDump);
    return DiffUtils.generateUnifiedDiff(left.toString(), right.toString(), leftDump, diff, 4);
  }
}
