use std::fmt::{self, Write};


/// a Compound is a part of a line with a consistent styling.
/// It can be part of word, several words, some inline code, or even the whole line.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Compound<'s> {
    pub src: &'s str,
    pub bold: bool,
    pub italic: bool,
    pub code: bool,
    pub strikeout: bool,
}

impl<'s> Compound<'s> {
    /// make a raw unstyled compound
    /// Involves no parsing
    #[inline(always)]
    pub fn raw_str(src: &'s str) -> Compound<'s> {
        Compound {
            src,
            bold: false,
            italic: false,
            code: false,
            strikeout: false,
        }
    }
    /// change the content but keeps the style arguments
    pub fn set_str(&mut self, src: &'s str) {
        self.src = src;
    }
    /// change the attributes by taking the values from the other
    /// compound, keeping the str
    pub fn set_attributes_from(&mut self, other: &Compound) {
        self.bold = other.bold;
        self.italic = other.italic;
        self.code = other.code;
        self.strikeout = other.strikeout;
    }
    /// return a sub part of the compound, with the same styling
    /// r_start is relative, that is 0 is the index of the first
    /// byte of this compound.
    #[inline(always)]
    pub fn sub(&self, r_start: usize, r_end: usize) -> Compound<'s> {
        Compound {
            src: &self.src[r_start..r_end],
            bold: self.bold,
            italic: self.italic,
            code: self.code,
            strikeout: self.strikeout,
        }
    }
    /// return a sub part of the compound, with the same styling
    /// r_start is relative, that is 0 is the index of the first
    /// char of this compound.
    ///
    /// The difference with `sub` is that this method is unicode
    /// aware and counts the chars instead of asking for the bytes
    #[inline(always)]
    pub fn sub_chars(&self, r_start: usize, r_end: usize) -> Compound<'s> {
        let mut rb_start = 0;
        let mut rb_end = 0;
        for (char_idx, (byte_idx, _)) in self.as_str().char_indices().enumerate() {
            if char_idx == r_start {
                rb_start = byte_idx;
            } else if char_idx == r_end {
                rb_end = byte_idx;
                break;
            }
        }
        if rb_end == 0 && rb_start != 0 {
            self.tail(rb_start)
        } else {
            self.sub(rb_start, rb_end)
        }
    }
    /// return a sub part at end of the compound, with the same styling
    /// r_start is relative, that is if you give 0 you get a clone of
    /// this compound
    #[inline(always)]
    pub fn tail(&self, r_start: usize) -> Compound<'s> {
        Compound {
            src: &self.src[r_start..],
            bold: self.bold,
            italic: self.italic,
            code: self.code,
            strikeout: self.strikeout,
        }
    }
    /// return a sub part at end of the compound, with the same styling
    /// r_start is relative, that is if you give 0 you get a clone of
    /// this compound
    ///
    /// The difference with `tail` is that this method is unicode
    /// aware and counts the chars instead of asking for the bytes
    #[inline(always)]
    pub fn tail_chars(&self, r_start: usize) -> Compound<'s> {
        let mut rb_start = 0;
        for (char_idx, (byte_idx, _)) in self.as_str().char_indices().enumerate() {
            rb_start = byte_idx;
            if char_idx == r_start {
                break;
            }
        }
        self.tail(rb_start)
    }

    // shortens this compound by `tail_size` bytes and returns the tail
    // as another compound
    pub fn cut_tail(&mut self, tail_size: usize) -> Compound<'s> {
        let cut = self.src.len() - tail_size;
        let tail = Compound {
            src: &self.src[cut..],
            bold: self.bold,
            italic: self.italic,
            code: self.code,
            strikeout: self.strikeout,
        };
        self.src = &self.src[0..cut];
        tail
    }

    // make a raw unstyled compound from part of a string
    // Involves no parsing
    #[inline(always)]
    pub fn raw_part(src: &'s str, start: usize, end: usize) -> Compound<'s> {
        Compound {
            src: &src[start..end],
            bold: false,
            italic: false,
            code: false,
            strikeout: false,
        }
    }
    #[inline(always)]
    pub fn new(
        src: &'s str, // the source string from which the compound is a part
        start: usize, // start index in bytes
        end: usize,
        bold: bool,
        italic: bool,
        code: bool,
        strikeout: bool,
    ) -> Compound<'s> {
        Compound {
            src: &src[start..end],
            italic,
            bold,
            code,
            strikeout,
        }
    }
    #[inline(always)]
    pub fn bold(mut self) -> Compound<'s> {
        self.bold = true;
        self
    }
    #[inline(always)]
    pub fn italic(mut self) -> Compound<'s> {
        self.italic = true;
        self
    }
    #[inline(always)]
    pub fn code(mut self) -> Compound<'s> {
        self.code = true;
        self
    }
    #[inline(always)]
    pub fn strikeout(mut self) -> Compound<'s> {
        self.strikeout = true;
        self
    }
    #[inline(always)]
    pub fn set_bold(&mut self, bold: bool) {
        self.bold = bold;
    }
    #[inline(always)]
    pub fn set_italic(&mut self, italic: bool) {
        self.italic = italic;
    }
    #[inline(always)]
    pub fn set_code(&mut self, code: bool) {
        self.code = code;
    }
    #[inline(always)]
    pub fn set_strikeout(&mut self, strikeout: bool) {
        self.strikeout = strikeout;
    }
    #[inline(always)]
    pub fn as_str(&self) -> &'s str {
        self.src
    }
    #[inline(always)]
    pub fn char_length(&self) -> usize {
        self.as_str().chars().count()
    }
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.src.is_empty()
    }
}

impl fmt::Display for Compound<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())?;
        Ok(())
    }
}

impl fmt::Debug for Compound<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.bold {
            f.write_char('B')?;
        }
        if self.italic {
            f.write_char('I')?;
        }
        if self.code {
            f.write_char('C')?;
        }
        if self.strikeout {
            f.write_char('S')?;
        }
        f.write_char('"')?;
        f.write_str(self.as_str())?;
        f.write_char('"')?;
        Ok(())
    }
}
