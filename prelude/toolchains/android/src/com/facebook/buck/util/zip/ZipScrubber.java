/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.buck.util.zip;

import static java.nio.file.StandardOpenOption.CREATE_NEW;

import com.facebook.buck.util.nio.LargeByteBuffer;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.base.Preconditions;
import java.io.IOException;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import org.kohsuke.args4j.Argument;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

/** Tool to eliminate non-deterministic or problematic bits of zip files. */
public class ZipScrubber {
  private ZipScrubber() {}

  private static final String USAGE =
      "usage: ZipScrubber [--paths-to-scrub <PATHS_TO_SCRUB_FILE] input_file"
          + " [scrubbed_file_output]";
  private static final long ZIP64_ENDCDL = 0x07064b50L;
  private static final int ZIP64_ENDCDL_OFFSET = -20;

  private static final int ZIP64_ENDTOT = 32; // 32 = 4 + 8 + 2 + 2 + 4 + 4 + 8
  private static final int ZIP64_ENDOFF = 48; // 48 = 4 + 8 + 2 + 2 + 4 + 4 + 8 + 8 + 8

  private static final int EXTENDED_TIMESTAMP_ID = 0x5455;
  private static final int EXTENDED_ZIP64_ID = 0x0001;

  private static void check(boolean expression, String msg) throws IOException {
    if (!expression) {
      throw new IOException(msg);
    }
  }

  public static void scrubZip(Path zipPath) throws IOException {
    try (FileChannel channel =
        FileChannel.open(zipPath, StandardOpenOption.READ, StandardOpenOption.WRITE)) {
      try (LargeByteBuffer.Scoped buffer = LargeByteBuffer.withFileChannel(channel)) {
        scrubZipBuffer(buffer);
      }
    }
  }

  @VisibleForTesting
  static void scrubZipBuffer(LargeByteBuffer map) throws IOException {
    map.order(ByteOrder.LITTLE_ENDIAN);

    // Search backwards from the end of the ZIP file, searching for the EOCD signature, which
    // designates the start of the EOCD.
    long eocdOffset = map.limit() - ZipEntry.ENDHDR;
    while (map.getInt(eocdOffset) != ZipEntry.ENDSIG) {
      eocdOffset--;
    }

    long cdEntries, cdOffset;
    // ZIP64 files can be identified by a separate ZIP64 EOCD section with a magic value.
    // See https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT for more details.
    final boolean isZip64 =
        eocdOffset + ZIP64_ENDCDL_OFFSET >= 0
            && map.getLong(eocdOffset + ZIP64_ENDCDL_OFFSET) == ZIP64_ENDCDL;

    if (isZip64) {
      // This is a ZIP64 format, and we need to look for fields in the ZIP64 EOCD.
      long zip64eocdOffset = map.getLong(eocdOffset + ZIP64_ENDCDL_OFFSET + 8);
      cdEntries = map.getLong(zip64eocdOffset + ZIP64_ENDTOT);
      cdOffset = map.getLong(zip64eocdOffset + ZIP64_ENDOFF);

    } else {
      // This is a ZIP32, and we can use the normal places for fields.
      cdEntries = Short.toUnsignedLong(map.getShort(eocdOffset + ZipEntry.ENDTOT));
      cdOffset = Integer.toUnsignedLong(map.getInt(eocdOffset + ZipEntry.ENDOFF));
    }

    ArrayList<Long> localHeaderOffsets = new ArrayList<>();

    // Go through each CD file entry and scrub them.
    for (long idx = 0; idx < cdEntries; idx++) {
      LargeByteBuffer entry = map.slice(cdOffset);
      check(entry.getInt(0) == ZipEntry.CENSIG, "expected central directory header signature");

      entry.putInt(ZipEntry.CENTIM, ZipConstants.DOS_FAKE_TIME);

      int filenameLength = entry.getShort(ZipEntry.CENNAM);
      int extrasLength = entry.getShort(ZipEntry.CENEXT);
      int commentLength = entry.getShort(ZipEntry.CENCOM);

      long compressedSize = Integer.toUnsignedLong(entry.getInt(ZipEntry.CENSIZ));
      long originalSize = Integer.toUnsignedLong(entry.getInt(ZipEntry.CENLEN));

      // Slice out the extra fields
      LargeByteBuffer extras =
          entry.position(ZipEntry.CENHDR + filenameLength).slice(0, extrasLength);

      // Keep track of the local headers for later...
      long headerOffset = Integer.toUnsignedLong(entry.getInt(ZipEntry.CENOFF));
      localHeaderOffsets.add(
          getLocalHeaderOffset(extras, isZip64, compressedSize, originalSize, headerOffset));

      scrubExtraFields(extras.position(0));

      cdOffset += ZipEntry.CENHDR + filenameLength + extrasLength + commentLength;
    }

    // Go back through the local headers spread around the archive and scrub them.
    for (long localHeaderOffset : localHeaderOffsets) {
      scrubLocalEntry(map.slice(localHeaderOffset));
    }
  }

  private static long getLocalHeaderOffset(
      LargeByteBuffer extras,
      boolean isZip64,
      long compressedSize,
      long originalSize,
      long headerOffset)
      throws IOException {
    if (isZip64) {
      /*
       * The ZIP64 extra is made up of:  (from ZIP spec section 4.5.3)
       *   - ID (1)                   2 bytes
       *   - Extra Data Size          2 bytes
       *   - Original Size            8 bytes
       *   - Compressed Size          8 bytes
       *   - Relative Header Offset   8 bytes
       *     ...
       *
       * The Original Size and Compressed Size are only in the extra if the value in the CD record
       * is 0xffffffff. So we need to check those sizes, and if they're there, skip them.
       */
      while (extras.position() < extras.limit() - 4) {
        int id = extras.getShort();
        int size = extras.getShort() & 0xFFFF;
        if (id == EXTENDED_ZIP64_ID) {
          int offset = 0;
          if (originalSize == 0xffffffffL) {
            offset += 8;
          }
          if (compressedSize == 0xffffffffL) {
            offset += 8;
          }
          if (headerOffset == 0xffffffffL) {
            return extras.getLong(extras.position() + offset);
          } else {
            return headerOffset;
          }
        }

        // Skip this extra.
        extras.position(extras.position() + size);
      }
    }

    return headerOffset;
  }

  private static void scrubLocalEntry(LargeByteBuffer entry) throws IOException {
    check(entry.getInt(0) == ZipEntry.LOCSIG, "expected local header signature");
    entry.putInt(ZipEntry.LOCTIM, ZipConstants.DOS_FAKE_TIME);
    scrubExtraFields(
        entry.slice(
            ZipEntry.LOCHDR + entry.getShort(ZipEntry.LOCNAM), entry.getShort(ZipEntry.LOCEXT)));
  }

  private static void scrubExtraFields(LargeByteBuffer data) throws IOException {
    // See http://mdfs.net/Docs/Comp/Archiving/Zip/ExtraField for structure of extra fields.
    //
    // Additionally, tools like zipalign inject zero values for padding, which seem to violate
    // the official zip spec.
    // zipalign README:
    // https://android.googlesource.com/platform/build/+/refs/tags/android-10.0.0_r33/tools/zipalign/README.txt
    // zipalign padding:
    // https://android.googlesource.com/platform/build/+/refs/tags/android-10.0.0_r33/tools/zipalign/ZipEntry.cpp#200

    final long end = data.limit();

    while (data.position() < end) {
      if (end - data.position() < 4) {
        // Check that all padding bytes are zero.
        int padding = 0;
        while (data.position() != end) {
          padding = (padding << 8) | (data.get() & 0xFF);
        }
        if (padding != 0) {
          throw new IllegalStateException("Non-zero padding " + padding);
        }
        break;
      }

      int id = data.getShort();
      int size = Short.toUnsignedInt(data.getShort());

      if (id == EXTENDED_TIMESTAMP_ID) {
        // 1 byte flag
        // 0-3 4-byte unix timestamps
        data.get(); // ignore flags
        size -= 1;
        while (size > 0) {
          data.putInt((int) (ZipConstants.getFakeTime() / 1000));
          size -= 4;
        }
      } else {
        if (id == 0 && size != 0) {
          // Padding should be length zero.
          throw new IllegalStateException("Non-zero length padding " + size);
        }
        if (data.position() + size >= end) {
          break;
        }
        data.position(data.position() + size);
      }
    }
  }

  /** Read the name of a zip file from a local entry. Useful for debugging. */
  @SuppressWarnings("unused")
  private static String localEntryName(LargeByteBuffer entry) throws IOException {
    byte[] nameBytes = new byte[entry.getShort(ZipEntry.LOCNAM)];
    entry.slice().position(ZipEntry.LOCHDR).get(nameBytes);
    return new String(nameBytes);
  }

  @Option(name = "--paths-to-scrub")
  private Path pathsToScrubFile;

  @Option(name = "--create-if-not-present")
  private boolean createIfNotPresent;

  @Argument private List<String> arguments = new ArrayList<>();

  public static void main(String[] args) throws IOException {
    ZipScrubber main = new ZipScrubber();
    CmdLineParser parser = new CmdLineParser(main);
    try {
      parser.parseArgument(args);
      main.run();
      System.exit(0);
    } catch (CmdLineException e) {
      System.err.println(e.getMessage());
      parser.printUsage(System.err);
      System.exit(1);
    }
  }

  public void run() throws IOException {
    Preconditions.checkState(pathsToScrubFile != null || !arguments.isEmpty(), USAGE);
    Preconditions.checkState(
        arguments.isEmpty() || arguments.size() == 1 || arguments.size() == 2, USAGE);
    if (pathsToScrubFile != null) {
      List<Path> pathsToScrub =
          Files.readAllLines(pathsToScrubFile).stream()
              .map(Paths::get)
              .collect(Collectors.toList());
      for (Path pathToScrub : pathsToScrub) {
        if (!Files.exists(pathToScrub)) {
          Preconditions.checkState(
              createIfNotPresent, "%s does not exist so cannot be scrubbed!", pathToScrub);
          createEmptyZip(pathToScrub);
        }

        Preconditions.checkState(pathToScrub.toFile().setWritable(true));
        scrubZip(pathToScrub);
      }
    }

    if (!arguments.isEmpty()) {
      Path inputZip = Paths.get(arguments.get(0));
      Preconditions.checkState(
          Files.exists(inputZip), "%s does not exist so cannot be scrubbed!", inputZip);

      Path scrubbedZip = inputZip;
      // Scrub in place if no output path
      if (arguments.size() == 2) {
        scrubbedZip = Paths.get(arguments.get(1));
        Files.copy(inputZip, scrubbedZip);
      }
      Preconditions.checkState(scrubbedZip.toFile().setWritable(true));
      scrubZip(scrubbedZip);
    }
  }

  private static void createEmptyZip(Path zipPath) throws IOException {
    ZipOutputStream zipOutputStream =
        new ZipOutputStream(Files.newOutputStream(zipPath, CREATE_NEW));
    zipOutputStream.close();
  }
}
