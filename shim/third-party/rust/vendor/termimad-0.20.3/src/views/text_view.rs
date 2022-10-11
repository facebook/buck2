use {
    crate::{
        area::Area,
        displayable_line::DisplayableLine,
        errors::Result,
        text::FmtText,
        SPACE_FILLING,
    },
    crossterm::{
        cursor::MoveTo,
        event::{
            KeyCode,
            KeyEvent,
            KeyModifiers,
        },
        queue,
        QueueableCommand,
        style::Print,
    },
    std::io::{stdout, Write},
};

/// A scrollable text, in a specific area.
///
/// The text is assumed to have been computed for the given area.
///
/// For example:
///
/// ```
/// use termimad::*;
///
/// // You typically borrow those 3 vars from elsewhere
/// let markdown = "#title\n* item 1\n* item 2";
/// let area = Area::new(0, 0, 10, 12);
/// let skin = MadSkin::default();
///
/// // displaying
/// let text = skin.area_text(markdown, &area);
/// let view = TextView::from(&area, &text);
/// view.write().unwrap();
/// ```
///
/// This struct is just a very thin wrapper and may
/// be created dynamically for renderings or event
/// handling.
///
/// If the text and skin are constant, you might prefer to
/// use a MadView instead of a TextView: the MadView owns
/// the markdown string and ensures the formatted text
/// is computed accordingly to the area.
pub struct TextView<'a, 't> {
    area: &'a Area,
    text: &'t FmtText<'t, 't>,
    pub scroll: usize, // number of lines hidden at start
    pub show_scrollbar: bool,
}

impl<'a, 't> TextView<'a, 't> {

    /// make a displayed text, that is a text in an area
    pub const fn from(area: &'a Area, text: &'t FmtText<'_, '_>) -> TextView<'a, 't> {
        TextView {
            area,
            text,
            scroll: 0,
            show_scrollbar: true,
        }
    }

    pub fn content_height(&self) -> usize {
        self.text.lines.len()
    }

    /// return an option which when filled contains
    ///  a tupple with the top and bottom of the vertical
    ///  scrollbar. Return none when the content fits
    ///  the available space (or if show_scrollbar is false).
    pub fn scrollbar(&self) -> Option<(u16, u16)> {
        if self.show_scrollbar {
            self.area.scrollbar(
                self.scroll as u16,
                self.content_height() as u16,
            )
        } else {
            None
        }
    }

    /// display the text in the area, taking the scroll into account.
    pub fn write(&self) -> Result<()> {
        let mut stdout = stdout();
        self.write_on(&mut stdout)?;
        stdout.flush()?;
        Ok(())
    }

    /// display the text in the area, taking the scroll into account.
    pub fn write_on<W: Write>(&self, w: &mut W) -> Result<()> {
        let scrollbar = self.scrollbar();
        let mut lines = self.text.lines.iter().skip(self.scroll as usize);
        let mut width = self.area.width as usize;
        if scrollbar.is_some() {
            width -= 1;
        }
        for j in 0..self.area.height {
            let y = self.area.top + j;
            w.queue(MoveTo(self.area.left, y))?;
            if let Some(line) = lines.next() {
                let dl = DisplayableLine::new(
                    self.text.skin,
                    line,
                    Some(width),
                );
                queue!(w, Print(&dl))?;
            } else {
                SPACE_FILLING.queue_styled(w, &self.text.skin.paragraph.compound_style, width)?;
            }
            if let Some((sctop, scbottom)) = scrollbar {
                if sctop <= y && y <= scbottom {
                    self.text.skin.scrollbar.thumb.queue(w)?;
                } else {
                    self.text.skin.scrollbar.track.queue(w)?;
                }
            }
        }
        Ok(())
    }

    /// set the scroll position but makes it fit into allowed positions.
    /// Return the actual scroll.
    pub fn set_scroll(&mut self, scroll: usize) -> usize {
        let area_height = self.area.height as usize;
        self.scroll = if self.content_height() > area_height {
            scroll.min(self.content_height() - area_height)
        } else {
            0
        };
        self.scroll
    }

    /// Change the scroll position.
    ///
    /// lines_count can be negative
    pub fn try_scroll_lines(&mut self, lines_count: i32) {
        if lines_count < 0 {
            let lines_count = -lines_count as usize;
                self.scroll = if lines_count >= self.scroll {
                0
            } else {
                self.scroll - lines_count
            };
        } else {
            self.set_scroll(self.scroll + lines_count as usize);
        }
    }

    /// change the scroll position
    /// pages_count can be negative
    pub fn try_scroll_pages(&mut self, pages_count: i32) {
        self.try_scroll_lines(pages_count * i32::from(self.area.height))
    }

    pub fn line_up(&mut self) -> bool {
        if self.scroll > 0 {
            self.scroll -= 1;
            true
        } else {
            false
        }
    }

    pub fn line_down(&mut self) -> bool {
        let content_height = self.content_height();
        let page_height = self.area.height as usize;
        if self.scroll + page_height < content_height {
            self.scroll += 1;
            true
        } else {
            false
        }
    }

    pub fn page_up(&mut self) -> bool {
        let page_height = self.area.height as usize;
        if self.scroll > page_height {
            self.scroll -= page_height;
            true
        } else if self.scroll > 0 {
            self.scroll = 0;
            true
        } else {
            false
        }
    }

    pub fn page_down(&mut self) -> bool {
        let content_height = self.content_height();
        let page_height = self.area.height as usize;
        if self.scroll + 2 * page_height < content_height {
            self.scroll += page_height;
            true
        } else if self.scroll + page_height < content_height {
            self.scroll = content_height - page_height;
            true
        } else {
            false
        }
    }

    /// Apply an event being a key: page_up, page_down, up and down.
    ///
    /// Return true when the event led to a change, false when it
    /// was discarded.
    pub fn apply_key_event(&mut self, key: KeyEvent) -> bool {
        if key.modifiers != KeyModifiers::NONE {
            return false;
        }
        match key.code {
            KeyCode::Up => self.line_up(),
            KeyCode::Down => self.line_down(),
            KeyCode::PageUp => self.page_up(),
            KeyCode::PageDown => self.page_down(),
            _ => false,
        }
    }
}
