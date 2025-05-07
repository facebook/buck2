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
use crossbeam_channel::Receiver;
use crossbeam_channel::Sender;
use crossbeam_channel::bounded;
use crossbeam_channel::unbounded;
use crossterm::tty::IsTty;

use crate::Dimensions;

/// Represents the target stream for output
#[derive(Copy, Clone, Debug)]
pub enum OutputTarget {
    /// Main output stream (default: stderr)
    Main,
    /// Auxiliary output stream (default: stdout)
    Aux,
}

pub trait IsTtyWrite: IsTty + Write {}

impl<T: IsTty + Write> IsTtyWrite for T {}

pub trait SuperConsoleOutput: Send + Sync + 'static {
    /// Called before rendering will occur. This has a chance to prevent rendering by returning
    /// false.
    fn should_render(&mut self) -> bool;

    /// Called to produce output. This may be called without should_render if we are finalizing or
    /// clearing. This should flush if possible.
    fn output(&mut self, buffer: Vec<u8>) -> anyhow::Result<()>;

    /// Called to produce to a specific output target.
    ///This may be called without should_render if we are finalizing or clearing. This should flush if possible.
    /// Default implementation sends all to main output for backwards compatibility
    fn output_to(&mut self, buffer: Vec<u8>, target: OutputTarget) -> anyhow::Result<()> {
        let _ = target;
        self.output(buffer)
    }

    /// Check if auxillary stream is tty
    fn aux_stream_is_tty(&self) -> bool {
        true
    }

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
    /// Auxiliary stream to write to.
    aux_stream: Box<dyn IsTtyWrite + Send + 'static + Sync>,
}

impl BlockingSuperConsoleOutput {
    pub fn new(
        stream: Box<dyn Write + Send + 'static + Sync>,
        aux_stream: Box<dyn IsTtyWrite + Send + 'static + Sync>,
    ) -> Self {
        Self { stream, aux_stream }
    }
}

impl SuperConsoleOutput for BlockingSuperConsoleOutput {
    fn should_render(&mut self) -> bool {
        true
    }

    fn output(&mut self, buffer: Vec<u8>) -> anyhow::Result<()> {
        self.output_to(buffer, OutputTarget::Main)
    }

    fn aux_stream_is_tty(&self) -> bool {
        self.aux_stream.is_tty()
    }

    fn output_to(&mut self, buffer: Vec<u8>, target: OutputTarget) -> anyhow::Result<()> {
        match target {
            OutputTarget::Main => {
                self.stream.write_all(&buffer)?;
                self.stream.flush()?;
            }
            OutputTarget::Aux => {
                self.aux_stream.write_all(&buffer)?;
                self.aux_stream.flush()?;
            }
        }
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
    sender: Sender<(Vec<u8>, OutputTarget)>,
    /// A channel back for errors encountered by the thread doing the writing.
    errors: Receiver<io::Error>,
    /// The thread doing the writing. It owns the other end of the aforementioned channels and will
    /// exit when the data sender is closed.
    handle: JoinHandle<()>,
    /// The auxillary output is compatible with tty
    aux_compatible: bool,
}

impl NonBlockingSuperConsoleOutput {
    pub fn new(
        stream: Box<dyn Write + Send + 'static + Sync>,
        aux_stream: Box<dyn IsTtyWrite + Send + 'static + Sync>,
    ) -> anyhow::Result<Self> {
        Self::new_for_writer(stream, aux_stream)
    }

    fn new_for_writer(
        mut stream: Box<dyn Write + Send + 'static + Sync>,
        mut aux_stream: Box<dyn IsTtyWrite + Send + 'static + Sync>,
    ) -> anyhow::Result<Self> {
        let (sender, receiver) = bounded::<(Vec<u8>, OutputTarget)>(1);
        let (error_sender, errors) = unbounded::<io::Error>();
        let aux_compatible = aux_stream.is_tty();

        let handle = std::thread::Builder::new()
            .name("superconsole-io".to_owned())
            .spawn(move || {
                for (data, output_target) in receiver.into_iter() {
                    let out_stream = match output_target {
                        OutputTarget::Main => &mut stream,
                        OutputTarget::Aux => &mut aux_stream as &mut dyn Write,
                    };
                    match out_stream.write_all(&data).and_then(|()| stream.flush()) {
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
            aux_compatible,
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
        self.output_to(buffer, OutputTarget::Main)
    }

    fn output_to(&mut self, buffer: Vec<u8>, target: OutputTarget) -> anyhow::Result<()> {
        if let Ok(err) = self.errors.try_recv() {
            return Err(anyhow::Error::from(err).context("Superconsole I/O thread errored"));
        }

        self.sender
            .send((buffer, target))
            .context("Superconsole I/O thread has crashed")?;

        Ok(())
    }

    fn aux_stream_is_tty(&self) -> bool {
        self.aux_compatible
    }

    /// Notify our writer thread that no further writes are expected. Wait for it to flush.
    fn finalize(self: Box<Self>) -> anyhow::Result<()> {
        let Self {
            sender,
            errors,
            handle,
            aux_compatible: _,
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
mod tests {
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

    impl IsTty for TestWriter {
        fn is_tty(&self) -> bool {
            true
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
        fn test_send_target(target0: OutputTarget, target1: OutputTarget) -> anyhow::Result<()> {
            let (writer, drain) = TestWriter::new();
            let aux_writer = writer.clone();

            let mut output = NonBlockingSuperConsoleOutput::new_for_writer(
                Box::new(writer),
                Box::new(aux_writer),
            )?;

            // Send a first message, this will go into write()
            assert!(output.should_render());
            output.output_to(msg(), target0)?;

            // Send a second message, this will stay in the channel.
            output.output_to(msg(), target1)?;

            // Now, kill the output
            assert!(!output.should_render());
            drop(drain);

            // We expect that should_render() will eventually return true.
            while !output.should_render() {
                std::thread::yield_now();
            }

            // Likewise, we expect that sending output and finalizing wold fail.
            assert!(output.output(Vec::new()).is_err());
            assert!(Box::new(output).finalize().is_err());

            Ok(())
        }

        // Test all combinations of targets
        for target0 in [OutputTarget::Main, OutputTarget::Aux] {
            for target1 in [OutputTarget::Main, OutputTarget::Aux] {
                test_send_target(target0, target1)?
            }
        }

        Ok(())
    }
}
