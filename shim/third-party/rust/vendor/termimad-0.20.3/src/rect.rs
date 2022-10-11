use {
    crate::{
        Area,
        CompoundStyle,
        errors::Result,
        SPACE_FILLING,
    },
    crossterm::{
        cursor,
        QueueableCommand,
    },
    std::io::Write,
};

#[derive(Debug)]
pub struct RectBorderStyle {
    top_left: char,
    top_right: char,
    bottom_right: char,
    bottom_left: char,
    top: char,
    right: char,
    bottom: char,
    left: char,
}

pub static BORDER_STYLE_HALF_WIDTH_OUTSIDE: &RectBorderStyle = &RectBorderStyle {
    top_left: '▛',
    top: '▀',
    top_right: '▜',
    bottom: '▄',
    bottom_right: '▟',
    right: '▐',
    bottom_left: '▙',
    left: '▌',
};

pub static BORDER_STYLE_MIDDLE_SQUARE_LINE: &RectBorderStyle = &RectBorderStyle {
    top_left: '┌',
    top: '─',
    top_right: '┐',
    bottom: '─',
    bottom_right: '┘',
    right: '│',
    bottom_left: '└',
    left: '│',
};

pub static BORDER_STYLE_MIDDLE_ROUND_LINE: &RectBorderStyle = &RectBorderStyle {
    top_left: '╭',
    top: '─',
    top_right: '╮',
    bottom: '─',
    bottom_right: '╯',
    right: '│',
    bottom_left: '╰',
    left: '│',
};

pub static BORDER_STYLE_BLAND: &RectBorderStyle = &RectBorderStyle {
    top_left: ' ',
    top: ' ',
    top_right: ' ',
    bottom: ' ',
    bottom_right: ' ',
    right: ' ',
    bottom_left: ' ',
    left: ' ',
};

/// A drawable rect, with various types of borders and an optional
/// filling.
///
/// There may be more types of border in the future, if somebody
/// asks for them
#[derive(Debug)]
pub struct Rect<'s> {
    pub area: Area,
    pub colors: CompoundStyle,
    pub fill: bool,
    pub border_style: &'s RectBorderStyle,
}

impl<'s> Rect<'s> {
    pub fn new(area: Area, colors: CompoundStyle) -> Self {
        Self {
            area,
            colors,
            fill: false,
            border_style: BORDER_STYLE_BLAND,
        }
    }
    pub fn set_border_style(&mut self, bs: &'s RectBorderStyle) {
        self.border_style = bs;
    }
    pub fn set_fill(&mut self, fill: bool) {
        self.fill = fill;
    }
    pub fn draw<W: Write>(&self, w: &mut W) -> Result<()> {
        let area = &self.area;
        let cs = &self.colors;
        let bs = &self.border_style;
        let mut y = area.top;
        w.queue(cursor::MoveTo(area.left, y))?;
        cs.queue(w, bs.top_left)?;
        if area.width > 2 {
            for _ in 0..area.width-2 {
                cs.queue(w, bs.top)?;
            }
        }
        cs.queue(w, bs.top_right)?;
        y += 1;
        while y < area.top + area.height - 1 {
            w.queue(cursor::MoveTo(area.left, y))?;
            cs.queue(w, bs.left)?;
            if self.fill {
                if area.width > 2 {
                    SPACE_FILLING.queue_styled(w, cs, area.width as usize - 2)?;
                }
            } else {
                w.queue(cursor::MoveTo(area.left + area.width - 1, y))?;
            }
            cs.queue(w, bs.right)?;
            y += 1;
        }
        w.queue(cursor::MoveTo(area.left, area.bottom() - 1))?;
        cs.queue(w, bs.bottom_left)?;
        if area.width > 2 {
            for _ in 0..area.width-2 {
                cs.queue(w, bs.bottom)?;
            }
        }
        cs.queue(w, bs.bottom_right)?;
        Ok(())
    }
}
