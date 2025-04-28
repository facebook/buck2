/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.jvm.java;

import com.facebook.buck.util.zip.JarBuilder;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.net.URI;

/**
 * An {@link JarFileObject} implementation that represents a {@link javax.tools.FileObject} that has
 * no operations and does not write the contents to any form of output.
 */
public class JavaNoOpFileObject extends JarFileObject {

  public JavaNoOpFileObject(URI uri, String pathInJar, Kind kind) {
    super(uri, pathInJar, kind);
  }

  @Override
  public InputStream openInputStream() {
    throw new UnsupportedOperationException();
  }

  @Override
  public OutputStream openOutputStream() {
    return new OutputStream() {
      @Override
      public void write(int b) {}

      @Override
      public void close() {}
    };
  }

  @Override
  public Reader openReader(boolean ignoreEncodingErrors) {
    throw new UnsupportedOperationException();
  }

  @Override
  public CharSequence getCharContent(boolean ignoreEncodingErrors) {
    throw new UnsupportedOperationException();
  }

  @Override
  public Writer openWriter() {
    return new OutputStreamWriter(openOutputStream());
  }

  @Override
  public void writeToJar(JarBuilder jarBuilder, String owner) {}
}
