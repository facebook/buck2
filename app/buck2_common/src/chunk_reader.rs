/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::buck2_env;
use buck2_error::BuckErrorContext;
use tokio::io::AsyncRead;
use tokio::io::AsyncReadExt;

/// A bit of a trivial utility to read AsyncRead in chunks before we send them to Manifold.
pub struct ChunkReader {
    chunk_size: u64,
}

impl ChunkReader {
    pub fn new() -> buck2_error::Result<Self> {
        let chunk_size = buck2_env!(
            "BUCK2_TEST_MANIFOLD_CHUNK_BYTES",
            type=u64,
            applicability=testing,
        )?
        .unwrap_or(8 * 1024 * 1024);
        Ok(ChunkReader { chunk_size })
    }

    pub async fn read<R>(&self, reader: &mut R) -> Result<Vec<u8>, buck2_error::Error>
    where
        R: AsyncRead + Unpin,
    {
        let mut buf = vec![];
        let mut reader = reader.take(self.chunk_size);
        let len = reader
            .read_to_end(&mut buf)
            .await
            .buck_error_context("Error reading chunk")?;
        buf.truncate(len);
        Ok(buf)
    }

    pub fn chunk_size(&self) -> u64 {
        self.chunk_size
    }
}
