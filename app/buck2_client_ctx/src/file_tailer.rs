/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Seek;
use std::io::SeekFrom;
use std::time::Duration;

use anyhow::Context;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use futures::FutureExt;
use tokio::sync::mpsc::UnboundedSender;
use tokio::sync::oneshot;

use crate::events_ctx::FileTailerEvent;
use crate::tokio_runtime_setup::client_tokio_runtime;

pub(crate) enum StdoutOrStderr {
    Stdout,
    Stderr,
}

/// When `tail_file()` is invoked, the FileTailer will open the file and seek
/// to the end. It'll then watch for changes to the file and copy any newly written
/// data to the writer.
///
/// When the tailer is dropped, it will do a final sync of the data to ensure that
/// the tail is up-to-date at that point.
pub(crate) struct FileTailer {
    // This thread is periodically checking the file for new data. When a message is
    // sent on the end_signaller, the thread will do one final sync of data and then exit.
    thread: Option<std::thread::JoinHandle<()>>,
    end_signaller: Option<oneshot::Sender<()>>,
}

impl Drop for FileTailer {
    fn drop(&mut self) {
        // If the thread has exited then don't error here.
        let _ignored = self.end_signaller.take().unwrap().send(());
        self.thread.take().unwrap().join().unwrap();
    }
}

impl FileTailer {
    pub(crate) fn tail_file(
        file: AbsNormPathBuf,
        sender: UnboundedSender<FileTailerEvent>,
        stdout_or_stderr: StdoutOrStderr,
    ) -> anyhow::Result<FileTailer> {
        let mut reader = BufReader::new(
            File::open(&file)
                .with_context(|| format!("Error setting up tailer for {}", file.display()))?,
        );

        reader.seek(SeekFrom::End(0))?;
        let (tx, rx) = tokio::sync::oneshot::channel();
        // Startup a thread that will repeatedly (with a 200ms interval between) copy from
        // the current position to the end of the file.
        // TODO(cjhopman): It would probably be nicer to implement this via inotify/fsevents/etc
        // rather than just repeatedly reading the file, but I tried to use each of
        // https://crates.io/crates/hotwatch and https://crates.io/crates/notify and neither worked.
        let thread = std::thread::spawn(move || {
            let runtime = client_tokio_runtime().unwrap();
            runtime.block_on(async move {
                let mut interval = tokio::time::interval(Duration::from_millis(200));
                let mut rx = rx.fuse();

                let mut completing = false;
                while !completing {
                    tokio::select! {
                        _ = interval.tick() => {},
                        _ = &mut rx => {
                            // This indicates that the FileTailer is being dropped.
                            // drain any remaining output and return.
                            completing = true;
                        }
                    }

                    let mut line = String::new();
                    while reader.read_line(&mut line).unwrap() != 0 {
                        let event = match stdout_or_stderr {
                            StdoutOrStderr::Stdout => FileTailerEvent::Stdout(line),
                            StdoutOrStderr::Stderr => FileTailerEvent::Stderr(line),
                        };
                        if sender.send(event).is_err() {
                            break;
                        }
                        line = String::new();
                    }
                }
            })
        });

        Ok(Self {
            end_signaller: Some(tx),
            thread: Some(thread),
        })
    }
}
