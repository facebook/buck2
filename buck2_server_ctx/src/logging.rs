/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs::File;
use std::io;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;

use gazebo::prelude::*;
use tracing_subscriber::fmt::MakeWriter;

static TRACING_LOG: Mutex<Option<TracingLogFile>> = Mutex::new(None);

#[derive(Clone, Dupe)]
pub struct TracingLogFile {
    file: Arc<PathBuf>,
    writer: Arc<Mutex<File>>,
}

impl TracingLogFile {
    pub fn new(file: PathBuf) -> anyhow::Result<Self> {
        let mut global = TRACING_LOG.lock().unwrap();
        Ok(if let Some(this) = &*global {
            this.dupe()
        } else {
            let writer = File::create(&file)?;

            let this = Self {
                file: Arc::new(file),
                writer: Arc::new(Mutex::new(writer)),
            };

            *global = Some(this.dupe());
            this
        })
    }

    pub fn refresh() -> anyhow::Result<()> {
        let this = TRACING_LOG.lock().unwrap();

        if let Some(this) = this.as_ref() {
            *this.writer.lock().unwrap() = File::create(&*this.file)?;
        }

        Ok(())
    }
}

impl<'a> io::Write for &'a TracingLogFile {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.writer.lock().unwrap().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer.lock().unwrap().flush()
    }
}

impl<'a> MakeWriter<'a> for TracingLogFile {
    type Writer = &'a Self;

    fn make_writer(&'a self) -> Self::Writer {
        self
    }
}
