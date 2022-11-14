/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! A data structure for tracking source positions in language implementations
//! The `CodeMap` tracks all source files and maps positions within them to linear indexes as if all
//! source files were concatenated. This allows a source position to be represented by a small
//! 32-bit `Pos` indexing into the `CodeMap`, under the assumption that the total amount of parsed
//! source code will not exceed 4GiB. The `CodeMap` can look up the source file, line, and column
//! of a `Pos` or `Span`, as well as provide source code snippets for error reporting.
use std::cmp;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Add;
use std::ops::Deref;
use std::ops::DerefMut;
use std::ptr;
use std::sync::Arc;

use allocative::Allocative;
use gazebo::prelude::*;

/// A small, `Copy`, value representing a position in a `CodeMap`'s file.
#[derive(
    Copy, Clone, Dupe, Hash, Eq, PartialEq, PartialOrd, Ord, Debug, Default, Allocative
)]
pub struct Pos(u32);

impl Pos {
    /// Constructor.
    pub const fn new(x: u32) -> Self {
        Self(x)
    }
}

impl Add<u32> for Pos {
    type Output = Pos;
    fn add(self, other: u32) -> Pos {
        Pos(self.0 + other)
    }
}

/// A range of text within a CodeMap.
#[derive(Copy, Dupe, Clone, Hash, Eq, PartialEq, Debug, Default, Allocative)]
pub(crate) struct Span {
    /// The position in the codemap representing the first byte of the span.
    begin: Pos,

    /// The position after the last byte of the span.
    end: Pos,
}

impl Span {
    /// Create a new span. Panics if `end < begin`.
    pub fn new(begin: Pos, end: Pos) -> Self {
        assert!(begin <= end);
        Span { begin, end }
    }

    /// The position at the first byte of the span.
    pub fn begin(self) -> Pos {
        self.begin
    }

    /// The position after the last byte of the span.
    pub fn end(self) -> Pos {
        self.end
    }

    /// The length in bytes of the text of the span
    pub fn len(self) -> u32 {
        self.end.0 - self.begin.0
    }

    /// Create a span that encloses both `self` and `other`.
    pub fn merge(self, other: Span) -> Span {
        Span {
            begin: cmp::min(self.begin, other.begin),
            end: cmp::max(self.end, other.end),
        }
    }

    /// Empty span in the end of this span.
    pub(crate) fn end_span(self) -> Span {
        Span {
            begin: self.end,
            end: self.end,
        }
    }

    /// Determines whether a `pos` is within this span.
    pub fn contains(self, pos: Pos) -> bool {
        self.begin <= pos && pos <= self.end
    }
}

/// Associate a Span with a value of arbitrary type (e.g. an AST node).
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub struct Spanned<T> {
    /// Data in the node.
    pub node: T,
    pub(crate) span: Span,
}

impl<T> Spanned<T> {
    /// Apply the function to the node, keep the span.
    pub fn into_map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            node: f(self.node),
            span: self.span,
        }
    }

    /// Apply the function to the node, keep the span.
    pub fn map<U>(&self, f: impl FnOnce(&T) -> U) -> Spanned<U> {
        Spanned {
            node: f(&self.node),
            span: self.span,
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.node
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.node
    }
}

// A cheap unowned unique identifier per file/CodeMap,
// somewhat delving into internal details.
// Remains unique because we take a reference to the CodeMap.
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy, Dupe)]
pub(crate) struct CodeMapId(*const ());

impl CodeMapId {
    pub(crate) const EMPTY: CodeMapId = CodeMapId(ptr::null());
}

#[derive(Clone, Dupe, Allocative)]
enum CodeMapImpl {
    Real(Arc<CodeMapData>),
    #[allocative(skip)]
    Native(&'static NativeCodeMap),
}

/// A data structure recording a source code file for position lookup.
#[derive(Clone, Dupe, Allocative)]
pub(crate) struct CodeMap(CodeMapImpl);

/// A `CodeMap`'s record of a source file.
#[derive(Allocative)]
struct CodeMapData {
    /// The filename as it would be displayed in an error message.
    filename: String,
    /// Contents of the file.
    source: String,
    /// Byte positions of line beginnings.
    lines: Vec<Pos>,
}

/// "Codemap" for `.rs` files.
pub(crate) struct NativeCodeMap {
    filename: &'static str,
    start: LineCol,
}

impl NativeCodeMap {
    const SOURCE: &'static str = "<native>";

    pub(crate) const FULL_SPAN: Span = Span {
        begin: Pos::new(0),
        end: Pos::new(Self::SOURCE.len() as u32),
    };

    pub(crate) const fn new(filename: &'static str, line: u32, column: u32) -> NativeCodeMap {
        Self {
            filename,
            start: LineCol {
                line: line as usize,
                column: column as usize,
            },
        }
    }

    pub(crate) const fn to_codemap(&'static self) -> CodeMap {
        CodeMap(CodeMapImpl::Native(self))
    }
}

impl Default for CodeMap {
    fn default() -> Self {
        Self::new("".to_owned(), "".to_owned())
    }
}

impl fmt::Debug for CodeMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "CodeMap({:?})", self.filename())
    }
}

impl PartialEq for CodeMap {
    /// Compares by identity
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Eq for CodeMap {}

impl Hash for CodeMap {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id().hash(state)
    }
}

impl CodeMap {
    /// Creates an new `CodeMap`.
    pub(crate) fn new(filename: String, source: String) -> CodeMap {
        let mut lines = vec![Pos(0)];
        lines.extend(source.match_indices('\n').map(|(p, _)| Pos(p as u32 + 1)));

        CodeMap(CodeMapImpl::Real(Arc::new(CodeMapData {
            filename,
            source,
            lines,
        })))
    }

    /// Only used internally for profiling optimisations
    pub(crate) fn id(&self) -> CodeMapId {
        match &self.0 {
            CodeMapImpl::Real(data) => CodeMapId(Arc::as_ptr(data) as *const ()),
            CodeMapImpl::Native(data) => CodeMapId(*data as *const NativeCodeMap as *const ()),
        }
    }

    pub(crate) fn full_span(&self) -> Span {
        let source = self.source();
        Span {
            begin: Pos(0),
            end: Pos(source.len() as u32),
        }
    }

    /// Gets the file and its line and column ranges represented by a `Span`.
    pub(crate) fn file_span(&self, span: Span) -> FileSpan {
        FileSpan {
            file: self.dupe(),
            span,
        }
    }

    /// Gets the name of the file
    pub fn filename(&self) -> &str {
        match &self.0 {
            CodeMapImpl::Real(data) => &data.filename,
            CodeMapImpl::Native(data) => data.filename,
        }
    }

    /// Gets the line number of a Pos.
    ///
    /// The lines are 0-indexed (first line is numbered 0)
    ///
    /// Panics if `pos` is not within this file's span.
    pub(crate) fn find_line(&self, pos: Pos) -> usize {
        assert!(pos <= self.full_span().end());
        match &self.0 {
            CodeMapImpl::Real(data) => match data.lines.binary_search(&pos) {
                Ok(i) => i,
                Err(i) => i - 1,
            },
            CodeMapImpl::Native(data) => data.start.line,
        }
    }

    /// Gets the line and column of a Pos.
    ///
    /// Panics if `pos` is not with this file's span or
    /// if `pos` points to a byte in the middle of a UTF-8 character.
    fn find_line_col(&self, pos: Pos) -> LineCol {
        assert!(pos <= self.full_span().end());
        match &self.0 {
            CodeMapImpl::Real(_) => {
                let line = self.find_line(pos);
                let line_span = self.line_span(line);
                let byte_col = pos.0 - line_span.begin.0;
                let column = self.source_span(line_span)[..byte_col as usize]
                    .chars()
                    .count();

                LineCol { line, column }
            }
            CodeMapImpl::Native(data) => LineCol {
                line: data.start.line,
                column: data.start.column + pos.0 as usize,
            },
        }
    }

    /// Gets the full source text of the file
    pub fn source(&self) -> &str {
        match &self.0 {
            CodeMapImpl::Real(data) => &data.source,
            CodeMapImpl::Native(_) => NativeCodeMap::SOURCE,
        }
    }

    /// Gets the source text of a Span.
    ///
    /// Panics if `span` is not entirely within this file.
    pub(crate) fn source_span(&self, span: Span) -> &str {
        &self.source()[(span.begin.0 as usize)..(span.end.0 as usize)]
    }

    /// Gets the span representing a line by line number.
    ///
    /// The line number is 0-indexed (first line is numbered 0). The returned span includes the
    /// line terminator.
    ///
    /// Panics if the line number is out of range.
    pub(crate) fn line_span(&self, line: usize) -> Span {
        match &self.0 {
            CodeMapImpl::Real(data) => {
                assert!(line < data.lines.len());
                Span {
                    begin: data.lines[line],
                    end: *data.lines.get(line + 1).unwrap_or(&self.full_span().end),
                }
            }
            CodeMapImpl::Native(data) => {
                assert_eq!(line, data.start.line);
                Span {
                    begin: Pos(0),
                    end: Pos(NativeCodeMap::SOURCE.len() as u32),
                }
            }
        }
    }

    pub(crate) fn resolve_span(&self, span: Span) -> ResolvedSpan {
        let begin = self.find_line_col(span.begin);
        let end = self.find_line_col(span.end);
        ResolvedSpan::from_span(begin, end)
    }

    /// Gets the source text of a line.
    ///
    /// The string returned does not include the terminating \r or \n characters.
    ///
    /// Panics if the line number is out of range.
    pub fn source_line(&self, line: usize) -> &str {
        self.source_span(self.line_span(line))
            .trim_end_matches(&['\n', '\r'][..])
    }

    pub(crate) fn source_line_at_pos(&self, pos: Pos) -> &str {
        self.source_line(self.find_line(pos))
    }
}

/// A line and column.
#[derive(Copy, Clone, Dupe, Hash, Eq, PartialEq, Debug)]
struct LineCol {
    /// The line number within the file (0-indexed).
    pub line: usize,

    /// The column within the line (0-indexed).
    pub column: usize,
}

/// A file, and a line and column range within it.
#[derive(Clone, Copy, Dupe, Eq, PartialEq, Debug)]
pub struct FileSpanRef<'a> {
    pub(crate) file: &'a CodeMap,
    pub(crate) span: Span,
}

/// A file, and a line and column range within it.
#[derive(Clone, Dupe, Eq, PartialEq, Debug, Hash, Allocative)]
pub struct FileSpan {
    pub(crate) file: CodeMap,
    pub(crate) span: Span,
}

impl<'a> fmt::Display for FileSpanRef<'a> {
    /// Formats the span as `filename:start_line:start_column: end_line:end_column`,
    /// or if the span is zero-length, `filename:line:column`, with a 1-indexed line and column.
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}:{}", self.file.filename(), self.resolve_span())
    }
}

impl fmt::Display for FileSpan {
    /// Formats the span as `filename:start_line:start_column: end_line:end_column`,
    /// or if the span is zero-length, `filename:line:column`, with a 1-indexed line and column.
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Display::fmt(&self.as_ref(), f)
    }
}

impl<'a> FileSpanRef<'a> {
    /// Filename of this reference.
    pub fn filename(&self) -> &str {
        self.file.filename()
    }

    /// Convert to the owned span.
    pub fn to_file_span(self) -> FileSpan {
        FileSpan {
            file: self.file.dupe(),
            span: self.span,
        }
    }

    /// Resolve span offsets to lines and columns.
    pub fn resolve_span(&self) -> ResolvedSpan {
        self.file.resolve_span(self.span)
    }
}

impl FileSpan {
    /// Filename of this span.
    pub fn filename(&self) -> &str {
        self.file.filename()
    }

    /// Resolve the span.
    pub fn source_span(&self) -> &str {
        self.file.source_span(self.span)
    }

    /// Cheap reference to the span.
    pub fn as_ref(&self) -> FileSpanRef {
        FileSpanRef {
            file: &self.file,
            span: self.span,
        }
    }

    /// Resolve the span to lines and columns.
    pub fn resolve_span(&self) -> ResolvedSpan {
        self.as_ref().resolve_span()
    }

    /// Resolve the span to lines and columns.
    pub fn resolve(&self) -> ResolvedFileSpan {
        ResolvedFileSpan {
            file: self.file.filename().to_owned(),
            span: self.file.resolve_span(self.span),
        }
    }
}

/// The locations of values within a span.
/// All are 0-based, but print out with 1-based.
#[derive(Debug, Dupe, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct ResolvedSpan {
    /// 0-based line number of the beginning of the span.
    pub begin_line: usize,
    /// 0-based column number of the beginning of the span.
    pub begin_column: usize,
    /// 0-based line number of the end of the span.
    pub end_line: usize,
    /// 0-based column number of the end of the span.
    pub end_column: usize,
}

impl Display for ResolvedSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let single_line = self.begin_line == self.end_line;
        let is_empty = single_line && self.begin_column == self.end_column;

        if is_empty {
            write!(f, "{}:{}", self.begin_line + 1, self.begin_column + 1)
        } else if single_line {
            write!(
                f,
                "{}:{}-{}",
                self.begin_line + 1,
                self.begin_column + 1,
                self.end_column + 1
            )
        } else {
            write!(
                f,
                "{}:{}-{}:{}",
                self.begin_line + 1,
                self.begin_column + 1,
                self.end_line + 1,
                self.end_column + 1
            )
        }
    }
}

impl From<ResolvedSpan> for lsp_types::Range {
    fn from(span: ResolvedSpan) -> Self {
        lsp_types::Range::new(
            lsp_types::Position::new(span.begin_line as u32, span.begin_column as u32),
            lsp_types::Position::new(span.end_line as u32, span.end_column as u32),
        )
    }
}

impl ResolvedSpan {
    fn from_span(begin: LineCol, end: LineCol) -> Self {
        Self {
            begin_line: begin.line,
            begin_column: begin.column,
            end_line: end.line,
            end_column: end.column,
        }
    }
}

/// File name and line and column pairs for a span.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ResolvedFileSpan {
    /// File name.
    pub file: String,
    /// The span.
    pub span: ResolvedSpan,
}

impl Display for ResolvedFileSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.file, self.span)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_codemap() {
        let source = "abcd\nefghij\nqwerty";
        let codemap = CodeMap::new("test1.rs".to_owned(), source.to_owned());
        let start = codemap.full_span().begin;

        // Test .name()
        assert_eq!(codemap.filename(), "test1.rs");

        // Test .find_line_col()
        assert_eq!(codemap.find_line_col(start), LineCol { line: 0, column: 0 });
        assert_eq!(
            codemap.find_line_col(start + 4),
            LineCol { line: 0, column: 4 }
        );
        assert_eq!(
            codemap.find_line_col(start + 5),
            LineCol { line: 1, column: 0 }
        );
        assert_eq!(
            codemap.find_line_col(start + 16),
            LineCol { line: 2, column: 4 }
        );

        // Test .source() and num lines.
        assert_eq!(codemap.source(), source);
        assert!(
            matches!(&codemap, CodeMap(CodeMapImpl::Real(codemap)) if codemap.lines.len() == 3)
        );

        // Test generic properties on each line
        for line in 0..3 {
            let line_str = codemap.source_line(line);
            let line_span = codemap.line_span(line);
            // The line_str omits trailing newlines
            assert_eq!(
                line_str.len() + if line < 2 { 1 } else { 0 },
                line_span.len() as usize
            );
            assert_eq!(line_str, source.lines().nth(line).unwrap());
            assert_eq!(codemap.find_line(line_span.begin), line);
            // The final character might be a newline, which is counted as the next line.
            // Not sure this is a good thing!
            let end = Pos(line_span.end().0 - 1);
            assert_eq!(codemap.find_line(end), line);
            assert_eq!(
                codemap.find_line_col(line_span.begin),
                LineCol { line, column: 0 }
            );
            assert_eq!(
                codemap.find_line_col(end),
                LineCol {
                    line,
                    column: line_span.len() as usize - 1
                }
            );
        }
    }

    #[test]
    fn test_multibyte() {
        let content = "65°00′N 18°00′W 汉语\n🔬";
        let codemap = CodeMap::new("<test>".to_owned(), content.to_owned());

        assert_eq!(
            codemap.find_line_col(codemap.full_span().begin + 21),
            LineCol {
                line: 0,
                column: 15
            }
        );
        assert_eq!(
            codemap.find_line_col(codemap.full_span().begin + 28),
            LineCol {
                line: 0,
                column: 18
            }
        );
        assert_eq!(
            codemap.find_line_col(codemap.full_span().begin + 33),
            LineCol { line: 1, column: 1 }
        );
    }

    #[test]
    fn test_line_col_span_display_point() {
        let line_col = LineCol { line: 0, column: 0 };
        let span = ResolvedSpan::from_span(line_col, line_col);
        assert_eq!(span.to_string(), "1:1");
    }

    #[test]
    fn test_line_col_span_display_single_line_span() {
        let begin = LineCol { line: 0, column: 0 };
        let end = LineCol {
            line: 0,
            column: 32,
        };
        let span = ResolvedSpan::from_span(begin, end);
        assert_eq!(span.to_string(), "1:1-33");
    }

    #[test]
    fn test_line_col_span_display_multi_line_span() {
        let begin = LineCol { line: 0, column: 0 };
        let end = LineCol {
            line: 2,
            column: 32,
        };
        let span = ResolvedSpan::from_span(begin, end);
        assert_eq!(span.to_string(), "1:1-3:33");
    }

    #[test]
    fn test_native_code_map() {
        static NATIVE_CODEMAP: NativeCodeMap = NativeCodeMap::new("test.rs", 100, 200);
        static CODEMAP: CodeMap = NATIVE_CODEMAP.to_codemap();
        assert_eq!(NativeCodeMap::SOURCE, CODEMAP.source());
        assert_eq!(NativeCodeMap::SOURCE, CODEMAP.source_line(100));
        assert_eq!(
            ResolvedSpan {
                begin_line: 100,
                begin_column: 200,
                end_line: 100,
                end_column: 200 + NativeCodeMap::SOURCE.len(),
            },
            CODEMAP.resolve_span(CODEMAP.full_span())
        );
    }
}
