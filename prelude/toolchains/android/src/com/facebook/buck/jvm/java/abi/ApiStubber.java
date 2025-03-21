/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java.abi;

import com.facebook.buck.core.filesystems.AbsPath;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

public class ApiStubber {

  private ApiStubber() {
    // Command line utility.
  }

  public static void main(String[] args) throws IOException {
    Path source = Paths.get(args[0]);
    Path destination = Paths.get(args[1]);
    boolean keepSynthetic = false;

    if (args.length > 2) {
      keepSynthetic = args[2].equals("--keep-synthetic");
    }

    AbsPath workingDirectory = AbsPath.of(Paths.get(".").toAbsolutePath().normalize());
    AbsPath sourceJar = toAbsPath(source, workingDirectory);
    AbsPath outputPath = toAbsPath(destination, workingDirectory);
    new StubJar(sourceJar, keepSynthetic).writeTo(outputPath);
  }

  private static AbsPath toAbsPath(Path path, AbsPath root) {
    return path.isAbsolute() ? AbsPath.of(path) : root.resolve(path);
  }
}
