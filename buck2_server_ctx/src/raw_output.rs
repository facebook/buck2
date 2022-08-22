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

use buck2_core::env_helper::EnvHelper;
use buck2_events::dispatch::EventDispatcher;
use gazebo::dupe::Dupe;

use crate::ctx::ServerCommandContextTrait;

pub struct RawOuputGuard<'a> {
    pub _phantom: PhantomData<&'a mut dyn ServerCommandContextTrait>,
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
        Ok(CHUNK_SIZE.get()?.unwrap_or(DEFAULT_CHUNK_SIZE))
    }
}

impl Write for RawOutputWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let len = std::cmp::min(buf.len(), self.chunk_size);
        if len > 0 {
            let raw_output = buck2_data::RawOutput {
                raw_output: String::from_utf8(buf[..len].to_vec()).map_err(|_| {
                    io::Error::new(io::ErrorKind::InvalidInput, "Output is not utf-8")
                })?,
            };
            self.dispatcher.instant_event(raw_output);
        }
        Ok(len)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}
