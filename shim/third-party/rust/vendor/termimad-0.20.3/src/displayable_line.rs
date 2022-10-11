use {
    crate::{
        line::FmtLine,
        skin::MadSkin,
    },
    std::fmt,
};

/// A facility to write just a line of a text.
///
/// It's normally not necessary to use this: it's more an internal
///  tool utility.
pub struct DisplayableLine<'s, 'l, 'p> {
    pub skin: &'s MadSkin,
    pub line: &'p FmtLine<'l>,
    pub width: Option<usize>, // available width
}

impl<'s, 'l, 'p> DisplayableLine<'s, 'l, 'p> {
    pub const fn new(skin: &'s MadSkin, line: &'p FmtLine<'l>, width: Option<usize>) -> Self {
        DisplayableLine { skin, line, width }
    }
}

impl fmt::Display for DisplayableLine<'_, '_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.skin.write_fmt_line(f, self.line, self.width, true)
    }
}
