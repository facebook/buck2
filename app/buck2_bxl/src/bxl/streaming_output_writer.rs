/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::io::BufWriter;
use std::io::Write;

use buck2_events::dispatch::EventDispatcher;

pub(crate) struct StreamingOutputWriter {
    inner: BufWriter<StreamingOutputInner>,
}

impl StreamingOutputWriter {
    pub(crate) fn new(dispatcher: EventDispatcher) -> Self {
        Self {
            inner: BufWriter::new(StreamingOutputInner { dispatcher }),
        }
    }
}

impl Write for StreamingOutputWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.inner.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.flush()
    }
}

struct StreamingOutputInner {
    dispatcher: EventDispatcher,
}

impl Write for StreamingOutputInner {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let msg = String::from_utf8_lossy(buf);
        self.dispatcher.streaming_output(msg.to_string());

        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
