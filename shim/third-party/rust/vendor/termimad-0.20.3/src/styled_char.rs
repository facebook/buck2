use {
    crate::{
        compound_style::CompoundStyle,
        errors::Result,
    },
    crossterm::{
        QueueableCommand,
        style::{Color, PrintStyledContent, StyledContent},
    },
    std::{
        fmt::{self, Display},
        io::Write,
    },
};

/// A modifiable character which can be easily written or repeated. Can
/// be used for bullets, horizontal rules or quote marks.
#[derive(Clone, Debug)]
pub struct StyledChar {
    compound_style: CompoundStyle,
    nude_char: char,
    styled_char: StyledContent<char>, // redundant, kept for performance
}

impl StyledChar {
    pub fn new(compound_style: CompoundStyle, nude_char: char) -> StyledChar {
        Self {
            nude_char,
            styled_char: compound_style.apply_to(nude_char),
            compound_style,
        }
    }
    pub fn nude(nude_char: char) -> StyledChar {
        Self::new(CompoundStyle::default(), nude_char)
    }
    pub fn from_fg_char(fg: Color, nude_char: char) -> StyledChar {
        Self::new(CompoundStyle::with_fg(fg), nude_char)
    }
    /// Change the char, keeping colors and attributes
    pub fn set_char(&mut self, nude_char: char) {
        self.nude_char = nude_char;
        self.styled_char = self.compound_style.apply_to(self.nude_char);
    }
    pub const fn get_char(&self) -> char {
        self.nude_char
    }
    /// Change the fg color, keeping the char, bg color and attributes
    pub fn set_fg(&mut self, color: Color) {
        self.compound_style.set_fg(color);
        self.styled_char = self.compound_style.apply_to(self.nude_char);
    }
    pub const fn get_fg(&self) -> Option<Color> {
        self.compound_style.get_fg()
    }
    /// Change the bg color, keeping the char, fg color and attributes
    pub fn set_bg(&mut self, color: Color) {
        self.compound_style.set_bg(color);
        self.styled_char = self.compound_style.apply_to(self.nude_char);
    }
    pub const fn get_bg(&self) -> Option<Color> {
        self.compound_style.get_bg()
    }
    /// Change the style (colors, attributes) of the styled char
    pub fn set_compound_style(&mut self, compound_style: CompoundStyle) {
        self.compound_style = compound_style;
        self.styled_char = self.compound_style.apply_to(self.nude_char);
    }
    /// Return a struct implementing `Display`, made of a (optimized) repetition
    ///  of the character with its style.
    pub fn repeated(&self, count: usize) -> StyledContent<String> {
        let mut s = String::new();
        for _ in 0..count {
            s.push(self.nude_char);
        }
        self.compound_style.apply_to(s)
    }
    pub fn queue_repeat<W: Write>(&self, w: &mut W, count: usize) -> Result<()> {
        let mut s = String::new();
        for _ in 0..count {
            s.push(self.nude_char);
        }
        self.compound_style.queue(w, s)
    }
    pub fn queue<W: Write>(&self, w: &mut W) -> Result<()> {
        w.queue(PrintStyledContent(self.styled_char))?;
        Ok(())
    }
    pub fn blend_with<C: Into<coolor::Color>>(&mut self, color: C, weight: f32) {
        self.compound_style.blend_with(color, weight);
    }
}

impl Display for StyledChar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.styled_char.fmt(f)
    }
}
