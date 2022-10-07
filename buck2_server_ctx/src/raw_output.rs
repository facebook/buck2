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
use std::marker::PhantomData;
use std::str;

use buck2_core::env_helper::EnvHelper;
use buck2_events::dispatch::EventDispatcher;
use gazebo::dupe::Dupe;

use crate::ctx::ServerCommandContextTrait;

pub struct RawOuputGuard<'a> {
    pub _phantom: PhantomData<&'a mut dyn ServerCommandContextTrait>,

    // `RawOutputWriter` expects arguments to `write` to be complete UTF-8 strings.
    //
    // `BufWriter` (this is implementation detail) may concatenate but never split
    // supplied inputs before passing to the underlying writer.
    //
    // So as long as complete UTF-8 strings are passed to this writer,
    // this writer will never pass partial UTF-8 strings to the underlying `RawOutputWriter`.
    pub inner: BufWriter<RawOutputWriter>,
}

/// A writer that fires InstantEvent (RawOutput) when `write` function is called.
/// Client is supposed to print the message to its stdout immediately as verbatim.
pub struct RawOutputWriter {
    dispatcher: EventDispatcher,
    /// Maximum bytes of a message that is delivered to cli per `write` call
    chunk_size: usize,
}

impl<'a> Write for RawOuputGuard<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.inner.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

impl<'a> Drop for RawOuputGuard<'a> {
    fn drop(&mut self) {
        // This would only happen if we had output that isn't utf-8 and got flushed. For now we live with ignoring
        // this.
        if let Err(e) = self.inner.flush() {
            tracing::error!("Discarded RawOutputWriter output: {:#}", e);
        }
    }
}

impl RawOutputWriter {
    pub fn new(context: &dyn ServerCommandContextTrait) -> anyhow::Result<Self> {
        Ok(Self {
            dispatcher: context.events().dupe(),
            chunk_size: RawOutputWriter::get_chunk_size()?,
        })
    }

    fn get_chunk_size() -> anyhow::Result<usize> {
        // protobuf recommends each message should be under 1MB
        const DEFAULT_CHUNK_SIZE: usize = 1024 * 1024;
        static CHUNK_SIZE: EnvHelper<usize> = EnvHelper::new("BUCK2_DEBUG_RAWOUTPUT_CHUNK_SIZE");
        Ok(CHUNK_SIZE.get_copied()?.unwrap_or(DEFAULT_CHUNK_SIZE))
    }

    /// Given complete valid UTF-8 string, truncate it to be no longer than given limit.
    fn truncate_str(s: &[u8], limit: usize) -> io::Result<&str> {
        fn err() -> io::Error {
            io::Error::new(io::ErrorKind::InvalidInput, "Output is not utf-8")
        }

        if limit == s.len() {
            // Input string must not be truncated, so special case for limit = len.
            str::from_utf8(s).map_err(|_| err())
        } else {
            match str::from_utf8(&s[..limit]) {
                Ok(s) => Ok(s),
                Err(e) => match e.error_len() {
                    None => {
                        // String is truncated in the middle of UTF-8 character.
                        //
                        // SAFETY: `e.valid_up_to()` is guaranteed to provide
                        // the valid utf8 string range.
                        unsafe { Ok(str::from_utf8_unchecked(&s[..e.valid_up_to()])) }
                    }
                    Some(_) => {
                        // Invalid UTF-8 sequence.
                        Err(err())
                    }
                },
            }
        }
    }
}

impl Write for RawOutputWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let len = std::cmp::min(buf.len(), self.chunk_size);
        if len > 0 {
            let s = RawOutputWriter::truncate_str(buf, len)?;
            if s.is_empty() {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    "Configured chunk size is not enough to hold a single utf-8 character",
                ));
            }
            let raw_output = buck2_data::RawOutput {
                raw_output: s.to_owned(),
            };
            self.dispatcher.instant_event(raw_output);
            Ok(s.len())
        } else {
            Ok(0)
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::raw_output::RawOutputWriter;

    #[test]
    fn test_truncate_str() {
        let s = "Маяк".as_bytes();
        assert_eq!("", RawOutputWriter::truncate_str(s, 0).unwrap());
        assert_eq!("", RawOutputWriter::truncate_str(s, 1).unwrap());
        assert_eq!("М", RawOutputWriter::truncate_str(s, 2).unwrap());
        assert_eq!("М", RawOutputWriter::truncate_str(s, 3).unwrap());
        assert_eq!("Ма", RawOutputWriter::truncate_str(s, 4).unwrap());
        assert_eq!("Ма", RawOutputWriter::truncate_str(s, 5).unwrap());
        assert_eq!("Мая", RawOutputWriter::truncate_str(s, 6).unwrap());
        assert_eq!("Мая", RawOutputWriter::truncate_str(s, 7).unwrap());
        assert_eq!("Маяк", RawOutputWriter::truncate_str(s, 8).unwrap());

        // Now test corrupted UTF-8.
        for i in 0..s.len() {
            let mut s0 = s.to_vec();
            // Invalid UTF-8 byte.
            s0[i] = 0xff;
            for j in 0..=s.len() {
                if j <= i {
                    assert!(RawOutputWriter::truncate_str(&s0, j).is_ok());
                } else {
                    assert!(RawOutputWriter::truncate_str(&s0, j).is_err());
                }
            }
        }
    }
}
