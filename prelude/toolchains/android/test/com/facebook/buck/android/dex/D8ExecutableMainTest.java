/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.buck.android.dex;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import com.facebook.buck.android.dex.D8ExecutableMain.DexRefCounts;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import org.junit.Test;

/**
 * Tests for DEX ref count reading functionality in D8ExecutableMain.
 *
 * <p>The DEX file format header contains exact method_ids_size, field_ids_size, and type_ids_size
 * at well-known offsets. These counts are used for 64K-limit-aware secondary DEX splitting.
 */
public class D8ExecutableMainTest {

  // First 120 bytes of a real DEX header from main_r_dot_java.jar, produced by D8.
  // Expected ref counts (independently verified via Python struct.unpack_from):
  //   type_ids_size   (offset 64) = 17
  //   field_ids_size  (offset 80) = 10
  //   method_ids_size (offset 88) = 19
  private static final byte[] REAL_DEX_HEADER = {
    0x64,
    0x65,
    0x78,
    0x0a,
    0x30,
    0x33,
    0x35,
    0x00, // magic: "dex\n035\0"
    (byte) 0xbf,
    (byte) 0xe4,
    0x3e,
    0x77, // checksum
    (byte) 0xef,
    0x59,
    (byte) 0xa5,
    (byte) 0xaa, // signature[0..3]
    0x43,
    (byte) 0xc0,
    (byte) 0x80,
    (byte) 0x8c, // signature[4..7]
    (byte) 0xb2,
    0x7e,
    (byte) 0xa9,
    0x0c, // signature[8..11]
    0x32,
    (byte) 0x9e,
    0x18,
    0x67, // signature[12..15]
    0x12,
    0x3b,
    (byte) 0xaa,
    (byte) 0xe6, // signature[16..19]
    0x64,
    0x0b,
    0x00,
    0x00, // file_size = 2916
    0x70,
    0x00,
    0x00,
    0x00, // header_size = 112
    0x78,
    0x56,
    0x34,
    0x12, // endian_tag = ENDIAN_CONSTANT
    0x00,
    0x00,
    0x00,
    0x00, // link_size = 0
    0x00,
    0x00,
    0x00,
    0x00, // link_off = 0
    (byte) 0x94,
    0x0a,
    0x00,
    0x00, // map_off
    0x2e,
    0x00,
    0x00,
    0x00, // string_ids_size = 46
    0x70,
    0x00,
    0x00,
    0x00, // string_ids_off
    0x11,
    0x00,
    0x00,
    0x00, // type_ids_size = 17      <-- offset 64
    0x28,
    0x01,
    0x00,
    0x00, // type_ids_off
    0x04,
    0x00,
    0x00,
    0x00, // proto_ids_size = 4
    0x6c,
    0x01,
    0x00,
    0x00, // proto_ids_off
    0x0a,
    0x00,
    0x00,
    0x00, // field_ids_size = 10     <-- offset 80
    (byte) 0x9c,
    0x01,
    0x00,
    0x00, // field_ids_off
    0x13,
    0x00,
    0x00,
    0x00, // method_ids_size = 19    <-- offset 88
    (byte) 0xec,
    0x01,
    0x00,
    0x00, // method_ids_off
    0x07,
    0x00,
    0x00,
    0x00, // class_defs_size = 7
    (byte) 0x84,
    0x02,
    0x00,
    0x00, // class_defs_off
    0x00,
    0x08,
    0x00,
    0x00, // data_size
    0x64,
    0x03,
    0x00,
    0x00, // data_off
    0x7e,
    0x05,
    0x00,
    0x00, // (beyond v40 header)
    (byte) 0x88,
    0x05,
    0x00,
    0x00, // (beyond v40 header)
  };

  @Test
  public void testReadDexRefCounts_realDexHeader() throws Exception {
    DexRefCounts counts =
        D8ExecutableMain.readDexRefCounts(new ByteArrayInputStream(REAL_DEX_HEADER));
    assertEquals("method_ids_size", 19, counts.methodIds);
    assertEquals("field_ids_size", 10, counts.fieldIds);
    assertEquals("type_ids_size", 17, counts.typeIds);
  }

  @Test
  public void testReadDexRefCounts_truncatedStream() throws Exception {
    byte[] truncated = new byte[50]; // less than 120-byte header
    try {
      D8ExecutableMain.readDexRefCounts(new ByteArrayInputStream(truncated));
      fail("Expected IOException for truncated stream");
    } catch (IOException e) {
      if (!e.getMessage().contains("header bytes")) {
        fail("Error message should mention header bytes, got: " + e.getMessage());
      }
    }
  }

  @Test
  public void testReadDexRefCounts_emptyStream() throws Exception {
    try {
      D8ExecutableMain.readDexRefCounts(new ByteArrayInputStream(new byte[0]));
      fail("Expected IOException for empty stream");
    } catch (IOException e) {
      // Expected
    }
  }
}
