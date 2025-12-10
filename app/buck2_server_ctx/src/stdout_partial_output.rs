/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::io;
use std::io::BufWriter;
use std::io::Write;

use crate::partial_result_dispatcher::PartialResultDispatcher;

/// A wrapper that implements Write for a PartialResultDispatcher that emits StdoutBytes.
pub struct StdoutPartialOutput<'a> {
    inner: BufWriter<WriterWrapper<'a>>,
}

impl<'a> StdoutPartialOutput<'a> {
    pub fn new(dispatcher: &'a mut PartialResultDispatcher<buck2_cli_proto::StdoutBytes>) -> Self {
        Self {
            inner: BufWriter::new(WriterWrapper { inner: dispatcher }),
        }
    }
}

impl Write for StdoutPartialOutput<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.inner.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

struct WriterWrapper<'a> {
    inner: &'a mut PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
}

impl Write for WriterWrapper<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        // Chunk writes to prevent gRPC transport failures with large messages.
        // Without this, very large responses can cause the stream to terminate
        // before sending the final CommandResult.
        const CHUNK_SIZE: usize = 128 * 1024 * 1024; // 128MB

        for chunk in buf.chunks(CHUNK_SIZE) {
            self.inner.emit(buck2_cli_proto::StdoutBytes {
                data: chunk.to_owned(),
            });
        }

        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}
