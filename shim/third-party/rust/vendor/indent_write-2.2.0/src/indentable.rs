use core::fmt::{self, Display, Formatter, Write};

use crate::fmt::IndentWriter;

/// Methods for adapting [`Display`] objects to indent themselves when printed.
pub trait Indentable: Sized + Display {
    /// Wrap this object so that its [`Display`] representation is indented
    /// with the given `indent`. Each non-empty line of the formatted output
    /// will be prefixed with the indent.
    ///
    /// # Example:
    ///
    /// ```
    /// use indent_write::indentable::Indentable;
    ///
    /// let content = "Line 1\nLine 2\n\nLine 3\n";
    /// let indented = content.indented("    ");
    /// let result = indented.to_string();
    ///
    /// assert_eq!(result, "    Line 1\n    Line 2\n\n    Line 3\n");
    /// ```
    #[must_use = "Indentables do nothing unless used"]
    fn indented(self, indent: &str) -> Indented<'_, Self> {
        Indented { item: self, indent }
    }

    /// Wrap this object so that its [`Display`] representation is indented
    /// with the given `indent`. Each non-empty line *except for the first*
    /// of the formatted output will be prefixed with the indent.
    ///
    /// # Example:
    ///
    /// ```
    /// use indent_write::indentable::Indentable;
    ///
    /// let content = "Line 1\nLine 2\n\nLine 3\n";
    /// let indented = content.indented_skip_initial("    ");
    /// let result = indented.to_string();
    ///
    /// assert_eq!(result, "Line 1\n    Line 2\n\n    Line 3\n");
    /// ```
    #[must_use = "Indentables do nothing unless used"]
    fn indented_skip_initial(self, indent: &str) -> IndentedSkipIntial<'_, Self> {
        IndentedSkipIntial { item: self, indent }
    }
}

impl<T: Display> Indentable for T {}

/// Wrapper struct that indents the [`Display`] representation of an item. When
/// printed with [`Display`], it will insert [`indent`][Self::indent] before
/// each non-empty line of the underlying [`item`][Self::item]'s [`Display`]
/// output.
///
/// Created with [`Indentable::indented`]; see its documentation for an example.
#[derive(Debug, Clone, Copy)]
pub struct Indented<'i, T: Display> {
    /// The item to indent.
    pub item: T,

    /// The indentation to insert before each non-empty line.
    pub indent: &'i str,
}

impl<T: Display> Display for Indented<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(IndentWriter::new(self.indent, f), "{}", self.item)
    }
}

/// Wrapper struct that indents the [`Display`] representation of an item. When
/// printed with [`Display`], it will insert [`indent`][Self::indent] before
/// each non-empty line _after the first_ of the underlying
/// [`item`][Self::item]'s [`Display`] output.
///
/// Created with [`Indentable::indented`]; see its documentation for an example.
#[derive(Debug, Clone, Copy)]
pub struct IndentedSkipIntial<'i, T: Display> {
    /// The item to indent.
    pub item: T,

    /// The indentation to insert before each non-empty line.
    pub indent: &'i str,
}

impl<T: Display> Display for IndentedSkipIntial<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            IndentWriter::new_skip_initial(self.indent, f),
            "{}",
            self.item
        )
    }
}
