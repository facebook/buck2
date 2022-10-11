use std::io;

use super::Inspect;

#[derive(Debug, Copy, Clone)]
enum IndentState<'a> {
    // We are currently writing a line. Forward writes until the end of the
    // line.
    MidLine,

    // An indent has been requested. Write empty lines, then write an indent
    // before the next non empty line.
    NeedIndent,

    // We are currently writing an indent.
    WritingIndent(&'a [u8]),
}

use IndentState::*;

/// Adapter for writers to indent each line
///
/// An `IndentWriter` adapts an [`io::Write`] object to insert an indent before
/// each non-empty line. Specifically, this means it will insert an indent
/// between each newline when followed by a non-newline.
///
/// These writers can be nested to provide increasing levels of indentation.
///
/// # Example
///
/// ```
/// # use std::io::Write;
/// use indent_write::io::IndentWriter;
///
/// let output = Vec::new();
///
/// let mut indented = IndentWriter::new("\t", output);
///
/// // Lines will be indented
/// write!(indented, "Line 1\nLine 2\n");
///
/// // Empty lines will not be indented
/// write!(indented, "\n\nLine 3\n\n");
///
/// assert_eq!(indented.get_ref(), b"\tLine 1\n\tLine 2\n\n\n\tLine 3\n\n");
/// ```
#[derive(Debug, Clone)]
pub struct IndentWriter<'i, W> {
    writer: W,
    indent: &'i str,
    state: IndentState<'i>,
}

impl<'i, W: io::Write> IndentWriter<'i, W> {
    /// Create a new [`IndentWriter`].
    pub fn new(indent: &'i str, writer: W) -> Self {
        Self {
            writer,
            indent,
            state: NeedIndent,
        }
    }

    /// Create a new [`IndentWriter`] which will not add an indent to the first
    /// written line.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::io::Write;
    /// use indent_write::io::IndentWriter;
    ///
    /// let mut buffer = Vec::new();
    /// let mut writer = IndentWriter::new_skip_initial("    ", &mut buffer);
    ///
    /// writeln!(writer, "Line 1").unwrap();
    /// writeln!(writer, "Line 2").unwrap();
    /// writeln!(writer, "Line 3").unwrap();
    ///
    /// assert_eq!(buffer, b"Line 1\n    Line 2\n    Line 3\n")
    /// ```
    #[inline]
    pub fn new_skip_initial(indent: &'i str, writer: W) -> Self {
        Self {
            writer,
            indent,
            state: MidLine,
        }
    }

    /// Extract the writer from the [`IndentWriter`], discarding any in-progress
    /// indent state.
    #[inline]
    pub fn into_inner(self) -> W {
        self.writer
    }

    /// Get a reference to the wrapped writer
    #[inline]
    pub fn get_ref(&self) -> &W {
        &self.writer
    }

    /// Get the string being used as an indent for each line
    #[inline]
    pub fn indent(&self) -> &'i str {
        self.indent
    }
}

impl<'i, W: io::Write> io::Write for IndentWriter<'i, W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        loop {
            match self.state {
                // We're currently writing a line. Scan for the end of the line.
                IndentState::MidLine => match buf.iter().position(|&b| b == b'\n') {
                    // No newlines in the input buffer, so write the entire thing.
                    None => break self.writer.write(buf),

                    // We are at a newline presently. Request an indent be
                    // written at the front of the next non-empty line, then
                    // continue looping (since we haven't yet attempted to
                    // write user data).
                    Some(0) => self.state = NeedIndent,

                    // There's an upcoming newline. Write out the remainder of
                    // this line, plus its newline. If the entire line was
                    // written, request an indent on the subsequent call to
                    // write.
                    Some(len) => {
                        break self.writer.write(&buf[..len + 1]).inspect(|&n| {
                            if n >= len {
                                self.state = NeedIndent;
                            }
                        })
                    }
                },

                // We need an indent. Scan for the next non-empty line.
                IndentState::NeedIndent => match buf.iter().position(|&b| b != b'\n') {
                    // No non-empty lines in the input buffer, so write the entire thing
                    None => break self.writer.write(buf),

                    // We are at the beginning of a non-empty line presently.
                    // Begin inserting an indent now, then continue looping
                    // (since we haven't yet attempted to write user data)
                    Some(0) => self.state = WritingIndent(self.indent.as_bytes()),

                    // There's an upcoming non-empty line. Write out the
                    // remainder of the empty lines. If all the empty lines
                    // were written, force an indent on the subsequent call to
                    // write.
                    Some(len) => {
                        break self.writer.write(&buf[..len]).inspect(|&n| {
                            if n >= len {
                                self.state = IndentState::WritingIndent(self.indent.as_bytes())
                            }
                        })
                    }
                },

                // We are writing an indent unconditionally. If we're in this
                // state, the input buffer is known to be the start of a non-
                // empty line.
                IndentState::WritingIndent(indent) => match self.writer.write(indent)? {
                    // We successfully wrote the entire indent. Continue with
                    // writing the input buffer.
                    n if n >= indent.len() => self.state = MidLine,

                    // Eof; stop work immediately
                    0 => break Ok(0),

                    // Only a part of the indent was written. Continue
                    // trying to write the rest of it, but update our state
                    // to keep it consistent in case the next write is an
                    // error
                    n => self.state = WritingIndent(&indent[n..]),
                },
            }
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        // If we're currently in the middle of writing an indent, flush it
        while let WritingIndent(ref mut indent) = self.state {
            match self.writer.write(*indent)? {
                // We wrote the entire indent. Proceed with the flush
                len if len >= indent.len() => self.state = MidLine,

                // EoF; return an error
                0 => return Err(io::ErrorKind::WriteZero.into()),

                // Partial write, continue writing.
                len => *indent = &indent[len..],
            }
        }

        self.writer.flush()
    }
}
