/// a composite is a group of compounds. It can be a whole line,
/// or a table cell

use crate::*;

/// The global style of a composite
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CompositeStyle {
    Paragraph,
    Header(u8), // never 0, and <= MAX_HEADER_DEPTH
    ListItem,
    Code,
    Quote,
}

/// a composite is a monoline sequence of compounds.
/// It's defined by
/// - the global style of the composite, if any
/// - a vector of styled parts
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Composite<'a> {
    pub style: CompositeStyle,
    pub compounds: Vec<Compound<'a>>,
}

impl<'a> From<Vec<Compound<'a>>> for Composite<'a> {
    fn from(compounds: Vec<Compound<'a>>) -> Composite<'a> {
        Composite {
            style: CompositeStyle::Paragraph,
            compounds,
        }
    }
}

impl Default for Composite<'_> {
    fn default() -> Self {
        Self {
            style: CompositeStyle::Paragraph,
            compounds: Vec::new(),
        }
    }
}

impl<'a> Composite<'a> {
    pub fn new() -> Composite<'a> {
        Self::default()
    }
    /// parse a monoline markdown snippet which isn't from a text.
    pub fn from_inline(md: &'a str) -> Composite<'a> {
        LineParser::from(md).inline()
    }
    #[inline(always)]
    pub fn is_code(&self) -> bool {
        matches!(self.style, CompositeStyle::Code { .. })
    }
    #[inline(always)]
    pub fn is_list_item(&self) -> bool {
        matches!(self.style, CompositeStyle::ListItem { .. })
    }
    #[inline(always)]
    pub fn is_quote(&self) -> bool {
        matches!(self.style, CompositeStyle::Quote { .. })
    }
    /// return the total number of characters in the composite
    ///
    /// Example
    /// ```rust
    /// assert_eq!(minimad::Line::from("τ:`2π`").char_length(), 4);
    /// ```
    ///
    /// This may not be the visible width: a renderer can
    ///  add some things (maybe some caracters) to wrap inline code,
    ///  or a bullet in front of a list item
    #[inline(always)]
    pub fn char_length(&self) -> usize {
        self.compounds
            .iter()
            .fold(0, |sum, compound| sum + compound.as_str().chars().count())
    }
    /// remove all white spaces at left, unless in inline code
    /// Empty compounds are cleaned out
    pub fn trim_start_spaces(&mut self) {
        loop {
            if self.compounds.is_empty() {
                break;
            }
            if self.compounds[0].code {
                break;
            }
            self.compounds[0].src = self.compounds[0]
                .src
                .trim_start_matches(char::is_whitespace);
            if self.compounds[0].is_empty() {
                self.compounds.remove(0);
            } else {
                break;
            }
        }
    }
    /// remove all white spaces at right, unless in inline code
    /// Empty compounds are cleaned out
    pub fn trim_end_spaces(&mut self) {
        loop {
            if self.compounds.is_empty() {
                break;
            }
            let last = self.compounds.len() - 1;
            if self.compounds[last].code {
                break;
            }
            self.compounds[last].src = self.compounds[last]
                .src
                .trim_end_matches(char::is_whitespace);
            if self.compounds[last].is_empty() {
                self.compounds.remove(last);
            } else {
                break;
            }
        }
    }
    pub fn trim_spaces(&mut self) {
        self.trim_start_spaces();
        self.trim_end_spaces();
    }
    pub fn is_empty(&self) -> bool {
        self.compounds.len() == 0
    }
    /// remove characters, and whole compounds if necessary
    pub fn remove_chars_left(&mut self, mut to_remove: usize) {
        while to_remove > 0 {
            if self.compounds.is_empty() {
                return;
            }
            let compound_len = self.compounds[0].char_length();
            if compound_len > to_remove {
                self.compounds[0] = self.compounds[0].tail_chars(to_remove);
                return;
            } else {
                self.compounds.remove(0);
                to_remove -= compound_len;
            }
        }
    }
    /// remove characters, and whole compounds if necessary
    pub fn remove_chars_right(&mut self, mut to_remove: usize) {
        while to_remove > 0 {
            if self.compounds.is_empty() {
                return;
            }
            let compound_idx = self.compounds.len() - 1;
            let compound_len = self.compounds[compound_idx].char_length();
            if compound_len > to_remove {
                self.compounds[compound_idx] = self.compounds[compound_idx].sub_chars(0, to_remove);
                return;
            } else {
                self.compounds.remove(compound_idx);
                to_remove -= compound_len;
            }
        }
    }

    /// remove characters, and whole compounds if necessary.
    ///
    /// align is the alignment of the composite. If the composite is left
    ///  aligned, we remove chars at the right.
    pub fn remove_chars(&mut self, to_remove: usize, align: Alignment) {
        match align {
            Alignment::Left => {
                self.remove_chars_right(to_remove);
            }
            Alignment::Right => {
                self.remove_chars_left(to_remove);
            }
            _ => {
                let to_remove_left = to_remove / 2;
                let to_remove_right = to_remove - to_remove_left;
                self.remove_chars_left(to_remove_left);
                self.remove_chars_right(to_remove_right);
            }
        }
    }
}

// Tests trimming composite
#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn composite_trim() {
        let mut left = Composite::from_inline(" *some* text  ");
        left.trim_spaces();
        assert_eq!(
            left,
            Composite {
                style: CompositeStyle::Paragraph,
                compounds: vec![
                    Compound::raw_str("some").italic(),
                    Compound::raw_str(" text"),
                ]
            }
        );
    }

    #[test]
    fn composite_trim_keep_code() {
        let mut left = Composite::from_inline(" ` `  ");
        left.trim_spaces();
        assert_eq!(
            left,
            Composite {
                style: CompositeStyle::Paragraph,
                compounds: vec![Compound::raw_str(" ").code(),]
            }
        );
    }

    #[test]
    fn empty_composite_trim() {
        let mut left = Composite::from_inline(" * * ** `` **  ");
        left.trim_start_spaces();
        assert_eq!(left.compounds.len(), 0);
    }

    #[test]
    fn composite_remove_chars() {
        let mut composite = Composite::from_inline(" *A* *B* `Test` *7*");
        composite.remove_chars_left(1);
        assert_eq!(composite.char_length(), 10);
        composite.remove_chars(5, Alignment::Right); // removes at left
        assert_eq!(composite.char_length(), 5);
        assert_eq!(
            composite.clone(),
            Composite {
                style: CompositeStyle::Paragraph,
                compounds: vec![
                    Compound::raw_str("est").code(),
                    Compound::raw_str(" "),
                    Compound::raw_str("7").italic(),
                ]
            },
        );
        composite.remove_chars_left(8);
        assert_eq!(composite.char_length(), 0);
        let mut composite = Composite::from_inline("`l'hélico` *est **rouge** vif!*");
        composite.remove_chars(15, Alignment::Center);
        assert_eq!(
            composite,
            Composite {
                style: CompositeStyle::Paragraph,
                compounds: vec![
                    Compound::raw_str("o").code(),
                    Compound::raw_str(" "),
                    Compound::raw_str("est ").italic(),
                    Compound::raw_str("rou").italic().bold(),
                ]
            },
        );
    }
}
