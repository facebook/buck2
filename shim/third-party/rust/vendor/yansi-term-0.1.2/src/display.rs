use std::{cell::Cell, fmt};

use crate::{Colour, Style};

/// An `DisplayANSI` includes a format function and a `Style`
struct DisplayANSI<F: FnOnce(&mut fmt::Formatter) -> fmt::Result> {
    style: Style,
    f: Cell<Option<F>>,
}

impl<F: FnOnce(&mut fmt::Formatter) -> fmt::Result> fmt::Display for DisplayANSI<F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let written = self.style.write_prefix(f)?;
        self.f.take().ok_or(fmt::Error).and_then(|c| c(f))?;
        if written {
            Style::write_reset(f)?;
        }
        Ok(())
    }
}

impl Style {
    /// Paints the given text with this style
    #[inline]
    pub fn paint<'a>(self, input: &'a str) -> impl fmt::Display + 'a {
        DisplayANSI {
            f: Cell::new(Some(move |f: &mut fmt::Formatter| f.write_str(input))),
            style: self,
        }
    }

    /// Paints the given format function with this style
    #[inline]
    pub fn paint_fn<F: FnOnce(&mut fmt::Formatter) -> fmt::Result>(
        self,
        f: F,
    ) -> impl fmt::Display {
        DisplayANSI {
            f: Cell::new(Some(f)),
            style: self,
        }
    }
}

impl Colour {
    /// Paints the given text with this colour
    /// This is a short-cut so you don’t have to use `Blue.normal()` just
    /// to get blue text.
    ///
    /// ```
    /// use yansi_term::Colour::Blue;
    /// println!("{}", Blue.paint("da ba dee"));
    /// ```
    #[inline]
    pub fn paint<'a>(self, input: &'a str) -> impl fmt::Display + 'a {
        self.normal().paint(input)
    }

    /// Paints the given format function with this colour
    #[inline]
    pub fn paint_fn<F: FnOnce(&mut fmt::Formatter) -> fmt::Result>(
        self,
        f: F,
    ) -> impl fmt::Display {
        self.normal().paint_fn(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_control_codes_for_plain() {
        let one = Style::default().paint("one");
        let two = Style::default().paint("two");
        let output = format!("{}{}", one, two);
        assert_eq!(output, "onetwo");
    }
}
