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
use buck2_core::fs::paths::AbsPathBuf;
use futures::FutureExt;
use tokio::runtime;
use tokio::sync::mpsc;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::oneshot;

/// When `tail_file()` is invoked, the FileTailer will open the file and seek
/// to the end. It'll then watch for changes to the file and copy any newly written
/// data to the writer.
///
/// When the tailer is dropped, it will do a final sync of the data to ensure that
/// the tail is up-to-date at that point.
pub struct FileTailer {
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
    pub fn tail_file(file: AbsPathBuf) -> anyhow::Result<(UnboundedReceiver<String>, Self)> {
        let mut reader = BufReader::new(
            File::open(&file)
                .with_context(|| format!("when setting up tailer for {}", file.display()))?,
        );

        // The capacity chosen below is the maximum number of lines the broadcast will buffer.
        let (sender, receiver) = mpsc::unbounded_channel();
        reader.seek(SeekFrom::End(0))?;
        let (tx, rx) = tokio::sync::oneshot::channel();
        // Startup a thread that will repeatedly (with a 200ms interval between) copy from
        // the current position to the end of the file.
        // TODO(cjhopman): It would probably be nicer to implement this via inotify/fsevents/etc
        // rather than just repeatedly reading the file, but I tried to use each of
        // https://crates.io/crates/hotwatch and https://crates.io/crates/notify and neither worked.
        let thread = std::thread::spawn(move || {
            let runtime = runtime::Builder::new_current_thread()
                .enable_time()
                .build()
                .unwrap();
            runtime.block_on(async move {
                let mut interval = tokio::time::interval(Duration::from_millis(200));
                let rx = rx.fuse();
                tokio::pin!(rx);

                let mut completing = false;
                while !completing {
                    tokio::select! {
                        _ = interval.tick() => {},
                        _ = &mut rx => {
                            // This indicates that the FileTailer is being droppped.
                            // drain any remaining output and return.
                            completing = true;
                        }
                    }

                    let mut line = String::new();
                    while reader.read_line(&mut line).unwrap() != 0 {
                        if sender.send(line).is_err() {
                            break;
                        }
                        line = String::new();
                    }
                }
            })
        });

        Ok((
            receiver,
            Self {
                end_signaller: Some(tx),
                thread: Some(thread),
            },
        ))
    }
}
