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

use dice_futures::cancellation::CancellationPoller;
use dice_futures::cancellation::CancelledError;

use crate::partial_result_dispatcher::PartialResultDispatcher;

// Buffer writes so that many individual small writes can't cause too many gRPC
// messages to be sent.
//
// Limit the size of the buffer, which implicitly chunks any writes to it to
// prevent gRPC transport failures with large messages. Without this, very large
// responses can cause the stream to terminate before sending the final
// CommandResult.
//
// Finally, keep the buffer size reasonable small so that clients handling these
// messages have a chance to detect client interrupts like Ctrl-C, and
// drop/cancel the command mid-output.
const BUFFER_CAPACITY: usize = 16 * 1024;

/// A wrapper that implements Write for a PartialResultDispatcher that emits StdoutBytes.
pub struct StdoutPartialOutput<'a> {
    inner: BufWriter<WriterWrapper<'a>>,
}

impl<'a> StdoutPartialOutput<'a> {
    pub fn new(
        dispatcher: &'a mut PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        cancellation: CancellationPoller,
    ) -> Self {
        Self {
            inner: BufWriter::with_capacity(
                BUFFER_CAPACITY,
                WriterWrapper {
                    inner: dispatcher,
                    cancellation,
                },
            ),
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
    cancellation: CancellationPoller,
}

impl WriterWrapper<'_> {
    fn cancelled() -> io::Error {
        io::Error::other(CancelledError)
    }
}

impl Write for WriterWrapper<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if self.cancellation.is_cancelled() {
            return Err(Self::cancelled());
        }

        self.inner.emit(buck2_cli_proto::StdoutBytes {
            data: buf.to_owned(),
        });

        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        if self.cancellation.is_cancelled() {
            return Err(Self::cancelled());
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use dice_futures::cancellation::CancelledError;

    use crate::stdout_partial_output::WriterWrapper;

    #[test]
    fn test_cancelled_error_is_wrapped() {
        let err = WriterWrapper::cancelled();
        assert!(
            err.get_ref()
                .is_some_and(|e| e.downcast_ref::<CancelledError>().is_some())
        );
    }
}
