use std::fmt::{self, Write};

use indent_write::fmt::IndentWriter;

// This is a wrapper for fmt::Write that only writes one char at a time, to test
// the invariants of IndentableWrite
#[derive(Debug, Clone)]
struct OneByteAtATime<W>(W);

impl<W: Write> Write for OneByteAtATime<W> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        s.chars().try_for_each(|c| self.0.write_char(c))
    }

    fn write_char(&mut self, c: char) -> fmt::Result {
        self.0.write_char(c)
    }
}

const CONTENT: &'static [&'static str] = &["\t😀 😀 😀", "\t\t😀 😀 😀", "\t😀 😀 😀"];

// Using a function to wrap a writer, run a standard test and check against expected
macro_rules! test_harness {
    ($target:ident => $transform:expr, expect: $result:expr) => {{
        let mut dest = String::new();

        {
            let $target = &mut dest;
            let mut indented_dest = $transform;
            for line in CONTENT {
                write!(&mut indented_dest, "{}\n", line).unwrap();
            }
        }

        assert_eq!(dest, $result);
    }};
}

#[test]
fn basic_test() {
    let mut dest = String::new();

    {
        let mut writer = IndentWriter::new("\t", &mut dest);
        for line in CONTENT {
            writeln!(writer, "{}", line).unwrap();
        }
    }

    assert_eq!(dest, "\t\t😀 😀 😀\n\t\t\t😀 😀 😀\n\t\t😀 😀 😀\n");
}

#[test]
fn test_prefix() {
    test_harness!(w => IndentWriter::new("    ", w), expect: "    \t😀 😀 😀\n    \t\t😀 😀 😀\n    \t😀 😀 😀\n")
}

#[test]
fn test_multi_indent() {
    let mut dest = String::new();

    writeln!(dest, "{}", "😀 😀 😀").unwrap();
    {
        let mut indent1 = IndentWriter::new("\t", &mut dest);
        writeln!(indent1, "{}", "😀 😀 😀").unwrap();
        {
            let mut indent2 = IndentWriter::new("\t", &mut indent1);
            writeln!(indent2, "{}", "😀 😀 😀").unwrap();
            {
                let mut indent3 = IndentWriter::new("\t", &mut indent2);
                writeln!(indent3, "{}", "😀 😀 😀").unwrap();
                writeln!(indent3, "").unwrap();
            }
            writeln!(indent2, "{}", "😀 😀 😀").unwrap();
        }
        writeln!(indent1, "{}", "😀 😀 😀").unwrap();
    }

    assert_eq!(
        dest,
        "😀 😀 😀
\t😀 😀 😀
\t\t😀 😀 😀
\t\t\t😀 😀 😀

\t\t😀 😀 😀
\t😀 😀 😀\n"
    )
}

// Technically this doesn't test anything in the crate, it just ensures that OneByteAtATime works
#[test]
fn test_partial_writes() {
    let mut dest = String::new();
    {
        let mut partial_writer = OneByteAtATime(&mut dest);
        write!(partial_writer, "Hello, {}!", "World").unwrap();
    }
    assert_eq!(dest, "Hello, World!");
}

#[test]
fn test_partial_simple_indent_writes() {
    let mut dest = String::new();
    {
        let writer = OneByteAtATime(&mut dest);
        let mut writer = IndentWriter::new("\t", writer);
        writeln!(writer, "{}", "Hello, World").unwrap();
        writeln!(writer, "{}", "😀 😀 😀\n😀 😀 😀").unwrap();
    }
    assert_eq!(dest, "\tHello, World\n\t😀 😀 😀\n\t😀 😀 😀\n");
}

#[test]
fn test_partial_simple_indent_writes_inverted() {
    let mut dest = String::new();
    {
        let writer = IndentWriter::new("\t", &mut dest);
        let mut writer = OneByteAtATime(writer);
        writeln!(writer, "{}", "Hello, World").unwrap();
        writeln!(writer, "{}", "😀 😀 😀\n😀 😀 😀").unwrap();
    }
    assert_eq!(dest, "\tHello, World\n\t😀 😀 😀\n\t😀 😀 😀\n");
}

#[test]
fn test_partial_writes_combined() {
    let mut dest = String::new();
    {
        let writer = OneByteAtATime(&mut dest);
        let writer = IndentWriter::new("    ", writer);
        let mut writer = OneByteAtATime(writer);

        writeln!(writer, "{}", "Hello, World").unwrap();
        writeln!(writer, "{}", "😀 😀 😀\n😀 😀 😀").unwrap();
    }
    assert_eq!(dest, "    Hello, World\n    😀 😀 😀\n    😀 😀 😀\n");
}
