use {
    crate::compound_style::CompoundStyle,
    crossterm::style::{Attribute, Color},
    minimad::Alignment,
    std::fmt,
};

/// A style applicable to a type of line.
///
/// It's made of
///  - the base style of the compounds
///  - the alignment
#[derive(Default, Clone, Debug)]
pub struct LineStyle {
    pub compound_style: CompoundStyle,
    pub align: Alignment,
}

impl LineStyle {
    /// Set the foreground color to the passed color.
    #[inline(always)]
    pub fn set_fg(&mut self, color: Color) {
        self.compound_style.set_fg(color);
    }

    /// Set the background color to the passed color.
    #[inline(always)]
    pub fn set_bg(&mut self, color: Color) {
        self.compound_style.set_bg(color);
    }

    /// Set the colors to the passed ones
    pub fn set_fgbg(&mut self, fg: Color, bg: Color) {
        self.compound_style.set_fgbg(fg, bg);
    }

    /// Add an `Attribute`. Like italic, underlined or bold.
    #[inline(always)]
    pub fn add_attr(&mut self, attr: Attribute) {
        self.compound_style.add_attr(attr);
    }

    /// Write a string several times with the line compound style
    #[inline(always)]
    pub fn repeat_string(&self, f: &mut fmt::Formatter<'_>, s: &str, count: usize) -> fmt::Result {
        self.compound_style.repeat_string(f, s, count)
    }

    /// Write 0 or more spaces with the line's compound style
    #[inline(always)]
    pub fn repeat_space(&self, f: &mut fmt::Formatter<'_>, count: usize) -> fmt::Result {
        self.repeat_string(f, " ", count)
    }

    pub fn blend_with<C: Into<coolor::Color>>(&mut self, color: C, weight: f32) {
        self.compound_style.blend_with(color, weight);
    }
}
