use {
    crate::{
        area::Area,
        errors::Result,
        skin::MadSkin,
        views::TextView,
    },
    crossterm::event::KeyEvent,
    std::io::Write,
};

/// A MadView is like a textview but it owns everything, from the
///  source markdown to the area and the skin, which often makes it more convenient
///  for dynamic texts.
/// It's also resizeable.
pub struct MadView {
    markdown: String,
    area: Area,
    pub skin: MadSkin,
    pub scroll: usize,
}

impl MadView {
    /// make a displayed text, that is a text in an area
    pub const fn from(markdown: String, area: Area, skin: MadSkin) -> MadView {
        MadView {
            markdown,
            area,
            skin,
            scroll: 0,
        }
    }
    /// render the markdown in the area, taking the scroll into
    /// account
    pub fn write(&self) -> Result<()> {
        self.write_on(&mut std::io::stdout())
    }
    pub fn write_on<W: Write>(&self, w: &mut W) -> Result<()> {
        let text = self.skin.area_text(&self.markdown, &self.area);
        let mut text_view = TextView::from(&self.area, &text);
        text_view.scroll = self.scroll;
        text_view.write_on(w)?;
        Ok(())
    }
    /// sets the new area. If it's the same as the precedent one,
    ///  this operation does nothing. The scroll is kept if possible.
    pub fn resize(&mut self, area: &Area) {
        if *area == self.area {
            return;
        }
        if area.width != self.area.width {
            self.scroll = 0; //TODO improve
        }
        self.area.left = area.left;
        self.area.top = area.top;
        self.area.height = area.height;
        self.area.width = area.width;
    }
    /// set the scroll amount.
    /// lines_count can be negative
    pub fn try_scroll_lines(&mut self, lines_count: i32) {
        let text = self.skin.area_text(&self.markdown, &self.area);
        let mut text_view = TextView::from(&self.area, &text);
        text_view.scroll = self.scroll;
        text_view.try_scroll_lines(lines_count);
        self.scroll = text_view.scroll;
    }
    /// set the scroll amount.
    /// lines_count can be negative
    pub fn try_scroll_pages(&mut self, pages_count: i32) {
        self.try_scroll_lines(pages_count * i32::from(self.area.height));
    }
    /// Apply an event being a key: page_up, page_down, up and down.
    ///
    /// Return true when the event led to a change, false when it
    /// was discarded.
    ///
    /// It's possible to handle the key yourself and call the try_scroll
    /// methods.
    pub fn apply_key_event(&mut self, key: KeyEvent) -> bool {
        let text = self.skin.area_text(&self.markdown, &self.area);
        let mut text_view = TextView::from(&self.area, &text);
        text_view.scroll = self.scroll;
        if text_view.apply_key_event(key) {
            self.scroll = text_view.scroll;
            true
        } else {
            false
        }
    }
}
