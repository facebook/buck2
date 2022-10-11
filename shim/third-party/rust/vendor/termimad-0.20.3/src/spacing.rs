use crate::{compound_style::CompoundStyle, errors::Result};
use minimad::Alignment;

#[derive(Debug, Clone, Copy)]
pub struct Spacing {
    pub width: usize,
    pub align: Alignment,
}

fn truncate(s: &str, max_chars: usize) -> &str {
    match s.char_indices().nth(max_chars) {
        None => s,
        Some((idx, _)) => &s[..idx],
    }
}

impl Spacing {
    /// compute the number of chars to add left and write of inner_width
    /// to fill outer_width
    #[inline(always)]
    pub const fn completions(align: Alignment, inner_width: usize, outer_width: usize) -> (usize, usize) {
        if inner_width >= outer_width {
            return (0, 0);
        }
        match align {
            Alignment::Left | Alignment::Unspecified => (0, outer_width - inner_width),
            Alignment::Center => {
                let lp = (outer_width - inner_width) / 2;
                (lp, outer_width - inner_width - lp)
            }
            Alignment::Right => (outer_width - inner_width, 0),
        }
    }
    #[inline(always)]
    pub const fn optional_completions(
        align: Alignment,
        inner_width: usize,
        outer_width: Option<usize>,
    ) -> (usize, usize) {
        match outer_width {
            Some(outer_width) => Spacing::completions(align, inner_width, outer_width),
            None => (0, 0),
        }
    }
    #[inline(always)]
    pub const fn completions_for(&self, inner_width: usize) -> (usize, usize) {
        Spacing::completions(self.align, inner_width, self.width)
    }
    pub fn write_counted_str<W>(
        &self,
        w: &mut W,
        s: &str,
        str_width: usize,
        style: &CompoundStyle,
    ) -> Result<()>
    where
        W: std::io::Write,
    {
        if str_width >= self.width {
            // we must truncate
            let s = truncate(s, self.width);
            style.queue_str(w, s)?;
        } else {
            // we must complete with spaces
            // This part could be written in a more efficient way
            let (lp, rp) = self.completions_for(str_width);
            let mut con = String::new();
            for _ in 0..lp {
                con.push(' ');
            }
            con.push_str(s);
            for _ in 0..rp {
                con.push(' ');
            }
            style.queue(w, con)?;
        }
        Ok(())
    }
    pub fn write_str<W>(&self, w: &mut W, s: &str, style: &CompoundStyle) -> Result<()>
    where
        W: std::io::Write,
    {
        self.write_counted_str(w, s, s.chars().count(), style)
    }
}
