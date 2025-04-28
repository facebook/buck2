/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.zip;

import static java.nio.file.StandardOpenOption.CREATE;
import static java.nio.file.StandardOpenOption.READ;
import static java.nio.file.StandardOpenOption.WRITE;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import javax.annotation.Nonnull;

/**
 * Alternative implementation to dependency packing, instead of copying several jars which is slow,
 * make use of a previous packed jar with immutable dependencies and append on top of it.
 *
 * @see AppendableCentralDirectory
 */
public class IncrementalJarOutputStreamImpl extends AppendingZipOutputStreamImpl {

  /**
   * Create a copy of a jar copying previous existing files and loading Central Directory to execute
   * append operation.
   *
   * @see AppendableCentralDirectory
   */
  public static IncrementalJarOutputStreamImpl createIncrementalJar(Path src, Path dst)
      throws IOException {

    final AppendableCentralDirectory centralDirectory =
        AppendableCentralDirectory.readCentralDirectory(src);

    try (final FileChannel srcChannel = FileChannel.open(src, READ)) {
      final FileChannel dstChannel = FileChannel.open(dst, CREATE, WRITE);

      // copy the block of files that exists in the input jar
      dstChannel.transferFrom(srcChannel, 0, centralDirectory.getEntryOffset());
      dstChannel.position(centralDirectory.getEntryOffset());

      return new IncrementalJarOutputStreamImpl(
          Channels.newOutputStream(dstChannel), centralDirectory);
    }
  }

  private final AppendableCentralDirectory appendableDirectory;

  IncrementalJarOutputStreamImpl(OutputStream stream) {
    this(stream, new AppendableCentralDirectory());
  }

  public IncrementalJarOutputStreamImpl(
      final OutputStream outputStream, final AppendableCentralDirectory appendableDirectory) {
    super(outputStream, false);
    this.appendableDirectory = appendableDirectory;
    setCurrentOffset(appendableDirectory.getEntryOffset());
  }

  @Nonnull
  @Override
  protected CentralDirectory getCentralDirectory() {
    return appendableDirectory;
  }
}
