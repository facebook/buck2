use core::fmt;
/// Adapter for writers to indent each line
///
/// An `IndentWriter` adapts a [`fmt::Write`] object to insert an indent before
/// each non-empty line. Specifically, this means it will insert an indent
/// between each newline when followed by a non-newline.
///
/// These writers can be nested to provide increasing levels of indentation.
///
/// # Example
///
/// ```
/// # use std::fmt::Write;
/// use indent_write::fmt::IndentWriter;
///
/// let output = String::new();
///
/// let mut indented = IndentWriter::new("\t", output);
///
/// // Lines will be indented
/// write!(indented, "Line 1\nLine 2\n");
///
/// // Empty lines will not be indented
/// write!(indented, "\n\nLine 3\n\n");
///
/// assert_eq!(indented.get_ref(), "\tLine 1\n\tLine 2\n\n\n\tLine 3\n\n");
/// ```
#[derive(Debug, Clone)]
pub struct IndentWriter<'i, W> {
    writer: W,
    indent: &'i str,
    need_indent: bool,
}

impl<'i, W: fmt::Write> IndentWriter<'i, W> {
    /// Create a new [`IndentWriter`].
    #[inline]
    pub fn new(indent: &'i str, writer: W) -> Self {
        Self {
            writer,
            indent,
            need_indent: true,
        }
    }

    /// Create a new [`IndentWriter`] which will not add an indent to the first
    /// written line.
    ///
    /// # Example
    ///
    /// ```
    /// # use std::fmt::Write;
    /// use indent_write::fmt::IndentWriter;
    ///
    /// let mut buffer = String::new();
    /// let mut writer = IndentWriter::new_skip_initial("    ", &mut buffer);
    ///
    /// writeln!(writer, "Line 1").unwrap();
    /// writeln!(writer, "Line 2").unwrap();
    /// writeln!(writer, "Line 3").unwrap();
    ///
    /// assert_eq!(buffer, "Line 1\n    Line 2\n    Line 3\n")
    /// ```
    #[inline]
    pub fn new_skip_initial(indent: &'i str, writer: W) -> Self {
        Self {
            writer,
            indent,
            need_indent: false,
        }
    }

    /// Extract the writer from the `IndentWriter`, discarding any in-progress
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

impl<'i, W: fmt::Write> fmt::Write for IndentWriter<'i, W> {
    fn write_str(&mut self, mut s: &str) -> fmt::Result {
        loop {
            match self.need_indent {
                // We don't need an indent. Scan for the end of the line
                false => match s.as_bytes().iter().position(|&b| b == b'\n') {
                    // No end of line in the input; write the entire string
                    None => break self.writer.write_str(s),

                    // We can see the end of the line. Write up to and including
                    // that newline, then request an indent
                    Some(len) => {
                        let (head, tail) = s.split_at(len + 1);
                        self.writer.write_str(head)?;
                        self.need_indent = true;
                        s = tail;
                    }
                },
                // We need an indent. Scan for the beginning of the next
                // non-empty line.
                true => match s.as_bytes().iter().position(|&b| b != b'\n') {
                    // No non-empty lines in input, write the entire string
                    None => break self.writer.write_str(s),

                    // We can see the next non-empty line. Write up to the
                    // beginning of that line, then insert an indent, then
                    // continue.
                    Some(len) => {
                        let (head, tail) = s.split_at(len);
                        self.writer.write_str(head)?;
                        self.writer.write_str(self.indent)?;
                        self.need_indent = false;
                        s = tail;
                    }
                },
            }
        }
    }

    fn write_char(&mut self, c: char) -> fmt::Result {
        // We need an indent, and this is the start of a non-empty line.
        // Insert the indent.
        if self.need_indent && c != '\n' {
            self.writer.write_str(self.indent)?;
            self.need_indent = false;
        }

        // This is the end of a non-empty line. Request an indent.
        if !self.need_indent && c == '\n' {
            self.need_indent = true;
        }

        self.writer.write_char(c)
    }
}
