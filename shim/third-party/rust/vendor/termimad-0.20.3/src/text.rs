use {
    crate::{
        code,
        line::FmtLine,
        skin::MadSkin,
        tbl,
        fit::wrap,
    },
    minimad::Text,
    std::fmt,
};

/// a formatted text, implementing Display
/// ```
/// use termimad::*;
/// let skin = MadSkin::default();
/// let my_markdown = "#title\n* item 1\n* item 2";
/// let text = FmtText::from(&skin, &my_markdown, Some(80));
/// println!("{}", &text);
/// ```
#[derive(Debug)]
pub struct FmtText<'k, 's> {
    pub skin: &'k MadSkin,
    pub lines: Vec<FmtLine<'s>>,
    pub width: Option<usize>, // available width
}

impl<'k, 's> FmtText<'k, 's> {
    /// build a displayable text for the specified width and skin
    ///
    /// This can be called directly or using one of the skin helper
    /// method.
    pub fn from(skin: &'k MadSkin, src: &'s str, width: Option<usize>) -> FmtText<'k, 's> {
        let mt = Text::from(src);
        Self::from_text(skin, mt, width)
    }
    /// build a fmt_text from a minimad text
    pub fn from_text(skin: &'k MadSkin, mut text: Text<'s>, width: Option<usize>) -> FmtText<'k, 's> {
        let mut lines = text
            .lines
            .drain(..)
            .map(|mline| FmtLine::from(mline, skin))
            .collect();
        tbl::fix_all_tables(&mut lines, width.unwrap_or(std::usize::MAX));
        code::justify_blocks(&mut lines);
        if let Some(width) = width {
            if width >= 3 {
                lines = wrap::hard_wrap_lines(lines, width)
                    .expect("width should be wide enough");
            }
        }
        FmtText { skin, lines, width }
    }
}

impl fmt::Display for FmtText<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for line in &self.lines {
            self.skin.write_fmt_line(f, line, self.width, false)?;
            writeln!(f)?;
        }
        Ok(())
    }
}
