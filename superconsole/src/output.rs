/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::io;
use std::io::Write;
use std::thread::JoinHandle;

use anyhow::Context as _;
use crossbeam_channel::bounded;
use crossbeam_channel::unbounded;
use crossbeam_channel::Receiver;
use crossbeam_channel::Sender;

use crate::Dimensions;

pub trait SuperConsoleOutput: Send + Sync + 'static {
    /// Called before rendering will occur. This has a chance to prevent rendering by returning
    /// false.
    fn should_render(&mut self) -> bool;

    /// Called to produce output. This may be called without should_render if we are finalizing or
    /// clearing. This should flush if possible.
    fn output(&mut self, buffer: Vec<u8>) -> anyhow::Result<()>;

    /// How big is the terminal to write to.
    fn terminal_size(&self) -> anyhow::Result<Dimensions> {
        Ok(crossterm::terminal::size()?.into())
    }

    /// Called when the console has finalized. This must block if necessary. No further output will
    /// be emitted.
    fn finalize(self: Box<Self>) -> anyhow::Result<()>;

    /// Get this Output as an Any. This is used for testing.
    fn as_any(&self) -> &dyn Any;

    /// Get this Output as a mutable Any. This is used for testing.
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

pub struct BlockingSuperConsoleOutput {
    /// Stream to write to.
    stream: Box<dyn Write + Send + 'static + Sync>,
}

impl BlockingSuperConsoleOutput {
    pub fn new(stream: Box<dyn Write + Send + 'static + Sync>) -> Self {
        Self { stream }
    }
}

impl SuperConsoleOutput for BlockingSuperConsoleOutput {
    fn should_render(&mut self) -> bool {
        true
    }

    fn output(&mut self, buffer: Vec<u8>) -> anyhow::Result<()> {
        self.stream.write_all(&buffer)?;
        self.stream.flush()?;

        Ok(())
    }

    fn finalize(self: Box<Self>) -> anyhow::Result<()> {
        Ok(())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

/// A non-blocking output for the SuperConsole. This makes a few guarantees:
///
/// - Calls to output() after should_render() returned true will not block.
/// - When finalize() returns, the last frame passed to output() is shown.
/// - When an error occurs, the next fallible call will return it.
pub(crate) struct NonBlockingSuperConsoleOutput {
    /// A channel to send frames for writing.
    sender: Sender<Vec<u8>>,
    /// A channel back for errors encountered by the thread doing the writing.
    errors: Receiver<io::Error>,
    /// The thread doing the writing. It owns the other end of the aforementioned channels and will
    /// exit when the data sender is closed.
    handle: JoinHandle<()>,
}

impl NonBlockingSuperConsoleOutput {
    pub fn new(stream: Box<dyn Write + Send + 'static + Sync>) -> anyhow::Result<Self> {
        Self::new_for_writer(stream)
    }

    fn new_for_writer(mut stream: Box<dyn Write + Send + 'static + Sync>) -> anyhow::Result<Self> {
        let (sender, receiver) = bounded::<Vec<u8>>(1);
        let (error_sender, errors) = unbounded::<io::Error>();

        let handle = std::thread::Builder::new()
            .name("superconsole-io".to_owned())
            .spawn(move || {
                for frame in receiver.into_iter() {
                    match stream.write_all(&frame).and_then(|()| stream.flush()) {
                        Ok(()) => {}
                        Err(e) => {
                            // This can only fail if the sender disconnected, in which case they'll
                            // stop sending us data momentarily, so ignore the failure.
                            let _ignored = error_sender.try_send(e);
                        }
                    }
                }
            })
            .context("Error spawning Superconsole I/O thread")?;

        Ok(Self {
            sender,
            errors,
            handle,
        })
    }
}

impl SuperConsoleOutput for NonBlockingSuperConsoleOutput {
    /// Check if we have free capacity in our channel. Note that if the channel is full, that means
    /// our writer thread already has 2 buffered frames (one in the channel, one it's currently
    /// writing out). In this case, refuse to produce further output.
    fn should_render(&mut self) -> bool {
        !self.errors.is_empty() || !self.sender.is_full()
    }

    /// Attempt to send out a frame. If we called should_render, this won't block. If we didn't,
    /// then it may block.
    fn output(&mut self, buffer: Vec<u8>) -> anyhow::Result<()> {
        if let Ok(err) = self.errors.try_recv() {
            return Err(anyhow::Error::from(err).context("Superconsole I/O thread errored"));
        }

        self.sender
            .send(buffer)
            .context("Superconsole I/O thread has crashed")?;

        Ok(())
    }

    /// Notify our writer thread that no further writes are expected. Wait for it to flush.
    fn finalize(self: Box<Self>) -> anyhow::Result<()> {
        let Self {
            sender,
            errors,
            handle,
        } = *self;
        drop(sender);

        let res = match errors.into_iter().next() {
            Some(err) => Err(anyhow::Error::from(err).context("Superconsole I/O thread errored")),
            None => Ok(()),
        };

        match handle.join() {
            Ok(()) => {}
            Err(panic) => std::panic::resume_unwind(panic),
        }

        res
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

#[cfg(test)]
mod test {
    use crossbeam_channel::Receiver;

    use super::*;

    /// A test writer that just sends into a channel. Lets us block / unblock the output to test
    /// for race conditions.
    #[derive(Clone)]
    struct TestWriter {
        sender: Sender<()>,
    }

    impl TestWriter {
        pub fn new() -> (Self, Receiver<()>) {
            let (sender, receiver) = bounded(0);
            (Self { sender }, receiver)
        }
    }

    impl Write for TestWriter {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            self.sender
                .send(())
                .map_err(|_| io::Error::new(io::ErrorKind::Other, "not writable"))?;

            Ok(buf.len())
        }

        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    fn msg() -> Vec<u8> {
        // Not an empty buffer, to ensure write() gets called.
        vec![1]
    }

    #[test]
    fn test_non_blocking_output_errors_on_next_output() -> anyhow::Result<()> {
        let (writer, drain) = TestWriter::new();

        let mut output = NonBlockingSuperConsoleOutput::new_for_writer(Box::new(writer))?;

        // Send a first message, this will go into write()
        assert!(output.should_render());
        output.output(msg())?;

        // Send a second message, this will stay in the channel.
        output.output(msg())?;

        // Now, kill the output
        assert!(!output.should_render());
        drop(drain);

        // We expect that should_render() will eventually return true.
        while !output.should_render() {
            std::thread::yield_now();
            continue;
        }

        // Likewise, we expect that sending output and finalizing wold fail.
        assert!(output.output(vec![]).is_err());
        assert!(Box::new(output).finalize().is_err());

        Ok(())
    }
}
