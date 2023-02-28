/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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

impl<'a> Write for StdoutPartialOutput<'a> {
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

impl<'a> Write for WriterWrapper<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.inner.emit(buck2_cli_proto::StdoutBytes {
            data: buf.to_owned(),
        });

        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}
