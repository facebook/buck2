use {
    super::*,
    crate::*,
    crossterm::{
        cursor,
        event::{
            Event,
            KeyCode,
            KeyEvent,
            KeyModifiers,
            MouseButton,
            MouseEvent,
            MouseEventKind,
        },
        queue,
        style::{
            Attribute,
            Color,
            SetBackgroundColor,
        },
    },
    std::io::Write,
};

/// A simple input field, managing its cursor position and
/// either handling the events you give it or being managed
/// through direct manipulation functions
/// (put_char, del_char_left, etc.).
///
/// To create a multiline input_field (otherwise called a
/// textarea) you should set an area with a height of more
/// than 1 and allow newline to be created on keyboard with
/// `new_line_on`.
pub struct InputField {
    content: InputFieldContent,
    area: Area,
    focused_style: CompoundStyle,
    unfocused_style: CompoundStyle,
    cursor_style: CompoundStyle,
    /// when true, the display will have stars instead of the normal chars
    pub password_mode: bool,
    /// if not focused, the content will be displayed as text
    focused: bool,
    scroll: Pos,
    new_line_keys: Vec<KeyEvent>,
}

impl Default for InputField {
    fn default() -> Self {
        Self::new(Area::uninitialized())
    }
}

macro_rules! wrap_content_fun {
    ($fun:ident) => {
        pub fn $fun(&mut self) -> bool {
            if self.content.$fun() {
                self.fix_scroll();
                true
            } else {
                false
            }
        }
    };
}

impl InputField {

    pub const ENTER: KeyEvent = KeyEvent {
        code: KeyCode::Enter,
        modifiers: KeyModifiers::NONE,
    };
    pub const ALT_ENTER: KeyEvent = KeyEvent {
        code: KeyCode::Enter,
        modifiers: KeyModifiers::ALT,
    };

    pub fn new(area: Area) -> Self {
        let focused_style = CompoundStyle::default();
        let unfocused_style = CompoundStyle::default();
        let mut cursor_style = focused_style.clone();
        cursor_style.add_attr(Attribute::Reverse);
        Self {
            content: InputFieldContent::default(),
            area,
            focused_style,
            unfocused_style,
            cursor_style,
            password_mode: false,
            focused: true,
            scroll: Pos::default(),
            new_line_keys: Vec::default(),
        }
    }
    pub fn set_mono_line(&mut self) {
        self.new_line_keys.clear();
    }
    /// define a key which will be interpreted as a new line.
    ///
    /// You may define several ones. If you set none, the input
    /// field will stay monoline unless you manage key events
    /// yourself to insert new lines.
    ///
    /// Beware that keys like Ctrl-Enter and Shift-Enter
    /// are usually received by TUI applications as simple Enter.
    ///
    /// Example:
    /// ```
    /// use termimad::*;
    /// let mut textarea = InputField::new(Area::new(5, 5, 20, 10));
    /// textarea.new_line_on(InputField::ALT_ENTER);
    /// ```
    pub fn new_line_on(&mut self, key: KeyEvent) {
        self.new_line_keys.push(key);
    }
    /// Change the area x, y and width, but not the height.
    ///
    /// Makes most sense for monoline inputs
    pub fn change_area(&mut self, x: u16, y: u16, w: u16) {
        if self.area.left != x || self.area.top != y || self.area.width != w {
            self.area.left = x;
            self.area.top = y;
            self.area.width = w;
            self.fix_scroll();
        }
    }
    pub fn set_area(&mut self, area: Area) {
        if self.area != area {
            self.area = area;
            self.fix_scroll();
        }
    }
    pub const fn area(&self) -> &Area {
        &self.area
    }
    /// return the current scrolling state on both axis
    pub const fn scroll(&self) -> Pos {
        self.scroll
    }
    /// Tell the input to be or not focused
    pub fn set_focus(&mut self, b: bool) {
        if self.focused == b {
            return;
        }
        self.focused = b;
        // there's no reason to change the scroll when unfocusing
        if self.focused {
            self.fix_scroll();
        }
    }
    pub const fn focused(&self) -> bool {
        self.focused
    }
    pub fn set_normal_style(&mut self, style: CompoundStyle) {
        self.focused_style = style;
        self.cursor_style = self.focused_style.clone();
        self.cursor_style.add_attr(Attribute::Reverse);
    }
    pub fn set_unfocused_style(&mut self, style: CompoundStyle) {
        self.unfocused_style = style;
    }
    pub const fn content(&self) -> &InputFieldContent {
        &self.content
    }
    pub fn get_content(&self) -> String {
        self.content.to_string()
    }
    pub fn is_empty(&self) -> bool {
        self.content.is_empty()
    }
    pub fn copy_selection(&mut self) -> String {
        self.content.selection_string()
    }
    pub fn cut_selection(&mut self) -> String {
        let s = self.content.selection_string();
        self.content.del_selection();
        self.fix_scroll();
        s
    }
    /// Write the given string in place of the selection, or
    /// insert the string if there's no wide selection.
    ///
    /// This is the usual behavior for pasting a string.
    pub fn replace_selection<S: AsRef<str>>(&mut self, s: S) {
        if self.content.has_wide_selection() {
            self.content.del_selection();
        }
        self.content.insert_str(s);
        self.fix_scroll();
    }
    /// tell whether the content of the input is equal
    ///  to the argument
    pub fn is_content(&self, s: &str) -> bool {
        self.content.is_str(s)
    }
    /// change the content to the new one and
    ///  put the cursor at the end **if** the
    ///  content is different from the previous one.
    pub fn set_str<S: AsRef<str>>(&mut self, s: S) {
        self.content.set_str(s);
        self.fix_scroll();
    }
    pub fn insert_new_line(&mut self) -> bool {
        self.content.insert_new_line();
        self.fix_scroll();
        true
    }
    /// put a char at cursor position (and increment this
    /// position).
    pub fn put_char(&mut self, c: char) -> bool {
        self.content.insert_char(c);
        self.fix_scroll();
        true
    }
    pub fn clear(&mut self) {
        self.content.clear();
        self.fix_scroll();
    }

    /// Insert the string on cursor point, as if it was typed
    pub fn insert_str<S: AsRef<str>>(&mut self, s: S) {
        self.content.insert_str(s);
        self.fix_scroll();
    }

    wrap_content_fun!(move_up);
    wrap_content_fun!(move_down);
    wrap_content_fun!(move_left);
    wrap_content_fun!(move_right);
    wrap_content_fun!(move_to_start);
    wrap_content_fun!(move_to_end);
    wrap_content_fun!(move_to_line_start);
    wrap_content_fun!(move_to_line_end);
    wrap_content_fun!(move_word_left);
    wrap_content_fun!(move_word_right);
    wrap_content_fun!(del_char_below);
    wrap_content_fun!(del_selection);
    wrap_content_fun!(del_char_left);
    wrap_content_fun!(del_word_left);
    wrap_content_fun!(del_word_right);
    wrap_content_fun!(move_current_line_up);
    wrap_content_fun!(move_current_line_down);
    wrap_content_fun!(select_word_around);

    pub fn page_up(&mut self) -> bool {
        if self.content.move_lines_up(self.area.height as usize) {
            self.fix_scroll();
            true
        } else {
            false
        }
    }

    pub fn page_down(&mut self) -> bool {
        if self.content.move_lines_down(self.area.height as usize) {
            self.fix_scroll();
            true
        } else {
            false
        }
    }

    /// apply an event being a key
    ///
    ///
    /// This function handles a few events like deleting a
    /// char, or going to the start (home key) or end (end key)
    /// of the input. If you want to totally handle events, you
    /// may call function like `put_char` and `del_char_left`
    /// directly.
    pub fn apply_key_event(&mut self, key: KeyEvent) -> bool {
        if !self.focused {
            return false;
        }
        if self.new_line_keys.contains(&key) {
            self.insert_new_line();
            return true;
        }
        use crossterm::event::{
            KeyModifiers as Mod,
        };
        match (key.code, key.modifiers) {
            (code, Mod::NONE) => self.apply_keycode_event(code, false),
            (code, Mod::SHIFT) => self.apply_keycode_event(code, true),
            _ => false,
        }
    }

    /// apply an event being a key without modifier.
    ///
    /// You don't usually call this function but the more
    /// general `apply_event`. This one is useful when you
    /// manage events mostly yourselves.
    pub fn apply_keycode_event(
        &mut self,
        code: KeyCode,
        shift: bool,
    ) -> bool {
        if code == KeyCode::Backspace {
            if self.content.has_wide_selection() {
                self.content.del_selection();
                true
            } else {
                self.content.del_char_left()
            }
        } else if code == KeyCode::Delete {
            if self.content.has_wide_selection() {
                self.content.del_selection();
                true
            } else {
                self.content.del_char_below()
            }
        } else {
            if shift {
                self.content.make_selection();
            } else {
                self.content.unselect();
            }
            match code {
                KeyCode::Home => self.move_to_line_start(),
                KeyCode::End => self.move_to_line_end(),
                KeyCode::Char(c) => self.put_char(c),
                KeyCode::Up => self.move_up(),
                KeyCode::Down => self.move_down(),
                KeyCode::Left => self.move_left(),
                KeyCode::PageUp => self.page_up(),
                KeyCode::PageDown => self.page_down(),
                KeyCode::Right => self.move_right(),
                _ => false,
            }
        }
    }

    /// Apply a simple left click event
    pub fn apply_click_event(&mut self, x: u16, y: u16) -> bool {
        if self.area.contains(x, y) {
            if self.focused {
                let y = ((y - self.area.top) as usize + self.scroll.y)
                    .min(self.content.line_count()-1);
                let line = &self.content.lines()[y];
                let x = line
                    .col_to_char_idx((x - self.area.left) as usize + self.scroll.x)
                    .unwrap_or(line.chars.len());
                self.content.set_cursor_pos(Pos { x, y });
            } else {
                self.focused = true;
            }
            true
        } else {
            false
        }
    }

    /// Apply a mouse event
    pub fn apply_mouse_event(
        &mut self,
        mouse_event: MouseEvent,
        is_double_click: bool,
    ) -> bool {
        let MouseEvent { kind, column, row, modifiers } = mouse_event;
        if self.area.contains(column, row) {
            if self.focused {
                let y = ((row - self.area.top) as usize + self.scroll.y)
                    .min(self.content.line_count()-1);
                let line = &self.content.lines()[y];
                let x = line
                    .col_to_char_idx((column - self.area.left) as usize + self.scroll.x)
                    .unwrap_or(line.chars.len());
                // We handle the selection click on down, so that it's set at the
                // start of drag.
                match kind {
                    MouseEventKind::Down(MouseButton::Left) => {
                        // FIXME Crossterm doesn't seem to send shift modifier
                        // with up or down mouse events
                        if modifiers == KeyModifiers::SHIFT {
                            self.content.make_selection();
                        } else {
                            self.content.unselect();
                        }
                        self.content.set_cursor_pos(Pos { x, y });
                    }
                    MouseEventKind::Up(MouseButton::Left) if is_double_click => {
                        self.content.set_cursor_pos(Pos { x, y });
                        self.content.select_word_around();
                    }
                    MouseEventKind::Drag(MouseButton::Left) => {
                        self.content.make_selection();
                        self.content.set_cursor_pos(Pos { x, y });
                    }
                    MouseEventKind::ScrollDown => {
                        self.scroll_down();
                    }
                    MouseEventKind::ScrollUp => {
                        self.scroll_up();
                    }
                    _ => {}
                }
            } else if matches!(kind, MouseEventKind::Down(MouseButton::Left)) {
                self.focused = true;
            }
            true
        } else {
            false
        }
    }

    /// apply the event to change the state (content, cursor)
    ///
    /// Return true when the event was used.
    pub fn apply_event(&mut self, event: Event, is_double_click: bool) -> bool {
        match event {
            Event::Mouse(mouse_event) => {
                self.apply_mouse_event(mouse_event, is_double_click)
            }
            Event::Key(KeyEvent{code, modifiers}) if self.focused => {
                if modifiers.is_empty() {
                    self.apply_keycode_event(code, false)
                } else if modifiers == KeyModifiers::SHIFT {
                    self.apply_keycode_event(code, true)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// apply the event to change the state (content, cursor, focus)
    ///
    /// Return true when the event was used.
    pub fn apply_timed_event(&mut self, event: TimedEvent) -> bool {
        self.apply_event(event.event, event.double_click)
    }

    pub fn scroll_up(&mut self) -> bool {
        if self.scroll.y > 0 {
            self.scroll.y -= 1;
            true
        } else {
            false
        }
    }

    pub fn scroll_down(&mut self) -> bool {
        let height = self.area.height as usize;
        let lines_len = self.content.line_count();
        if self.scroll.y + height < lines_len {
            self.scroll.y += 1;
            true
        } else {
            false
        }
    }

    fn fix_scroll(&mut self) {
        let mut width = self.area.width as usize;
        let height = self.area.height as usize;
        let lines = &self.content.lines();
        let has_y_scroll = lines.len() > height;
        if has_y_scroll {
            width -= 1;
        } else {
            self.scroll.y = 0;
        }
        let pos = self.content.cursor_pos();

        if has_y_scroll {
            if self.scroll.y + height > lines.len() {
                self.scroll.y = lines.len() - height;
            }
            if self.focused {
                // we must ensure the cursor is visible
                if self.scroll.y > pos.y {
                    self.scroll.y = pos.y;
                    if self.scroll.y > 0 && height > 4 {
                        self.scroll.y -= 1;
                    }
                } else if pos.y >= self.scroll.y + height {
                    self.scroll.y = pos.y - height + 1;
                    if pos.y + 1 < lines.len() {
                        self.scroll.y -= 1;
                    }
                }
            }
        }

        let line = self.content.current_line();
        let line_width = line.width();
        if line_width < width {
            self.scroll.x = 0;
        } else {
            if self.focused {
                // we don't show ellipsis if the width is below 4
                // so we need less margin
                if width < 4 {
                    if pos.x < 2 {
                        self.scroll.x = 0;
                    } else if pos.x < self.scroll.x + 1 {
                        self.scroll.x = pos.x - 1;
                    } else if pos.x > self.scroll.x + width {
                        self.scroll.x = pos.x + 1 - width;
                    }
                } else {
                    let wpx = line.char_idx_to_col(pos.x);
                    if wpx < self.scroll.x + 2 {
                        if wpx < 2 {
                            self.scroll.x = 0;
                        } else {
                            self.scroll.x = wpx - 2;
                        }
                    } else if wpx > self.scroll.x + width - 2 {
                        self.scroll.x = wpx + 2 - width;
                    }
                }
            }
            if self.scroll.x + width > line_width + 1 {
                self.scroll.x = line_width + 1 - width;
            }
        }
    }

    /// Render the input field on screen.
    ///
    /// All rendering must be explicitely called, no rendering is
    /// done on functions changing the state.
    ///
    /// w is typically either stderr or stdout. This function doesn't
    /// flush by itself (useful to avoid flickering)
    pub fn display_on<W: Write>(&self, w: &mut W) -> Result<(), Error> {
        let normal_style = if self.focused {
            &self.focused_style
        } else {
            &self.unfocused_style
        };

        let mut width = self.area.width as usize;
        let pos = self.content.cursor_pos();
        let scrollbar = self.area.scrollbar(
            self.scroll.y as u16,
            self.content.line_count() as u16,
        );
        if scrollbar.is_some() {
            width -= 1;
        }

        queue!(w, SetBackgroundColor(Color::Reset))?;
        let mut scrollbar_style = &crate::get_default_skin().scrollbar;
        let mut focused_scrollbar_style;
        if self.focused {
            if let Some(bg) = self.focused_style.get_bg() {
                focused_scrollbar_style = scrollbar_style.clone();
                focused_scrollbar_style.set_bg(bg);
                scrollbar_style = &focused_scrollbar_style;
            }
        }

        let mut numbered_lines = self.content.lines().iter()
            .map(|line| &line.chars)
            .enumerate()
            .skip(self.scroll.y);

        let selection = self.content.selection();

        for j in 0..self.area.height {
            queue!(w, cursor::MoveTo(self.area.left, j + self.area.top))?;
            if let Some((y, chars)) = numbered_lines.next() {
                let cursor_at_end = self.focused && y == pos.y && pos.x == chars.len();
                let mut width_to_skip = self.scroll.x;
                let mut skipped_width = 0;
                let mut displayed_width = 0;
                let mut width = width; // available width for rendering chars
                // we don't show ellipsis if the width is 4 or less
                if width_to_skip > 0 && width > 4 {
                    normal_style.queue(w, fit::ELLIPSIS)?;
                    width_to_skip += 1;
                    width -= 1;
                }
                for (i, c) in chars.iter().enumerate() {
                    let c = if self.password_mode { '*' } else { *c };
                    let char_width = InputFieldContent::char_width(c);
                    if skipped_width < width_to_skip {
                        // char hidden by scroll on x
                        skipped_width += char_width;
                        continue;
                    }
                    if displayed_width + char_width >= width {
                        let is_last = i == chars.len() - 1;
                        if !is_last || displayed_width + char_width > width {
                            if self.focused && selection.contains(i, y) {
                                self.cursor_style.queue(w, fit::ELLIPSIS)?;
                            } else {
                                normal_style.queue(w, fit::ELLIPSIS)?;
                            }
                            displayed_width += 1;
                            break;
                        }
                    }
                    if self.focused && selection.contains(i, y) {
                        self.cursor_style.queue(w, c)?;
                    } else {
                        normal_style.queue(w, c)?;
                    }
                    displayed_width += char_width;
                    if displayed_width >= width {
                        break;
                    }
                }
                if displayed_width < width && cursor_at_end {
                    self.cursor_style.queue(w, ' ')?;
                    displayed_width += 1;
                }
                while displayed_width < width {
                    normal_style.queue(w, ' ')?;
                    displayed_width += 1;
                }
            } else {
                SPACE_FILLING.queue_styled(w, normal_style, width)?;
            }
            if let Some((sctop, scbottom)) = scrollbar {
                let y = j + self.area.top;
                if sctop <= y && y <= scbottom {
                    scrollbar_style.thumb.queue(w)?;
                } else {
                    scrollbar_style.track.queue(w)?;
                }
            }
        }
        Ok(())
    }

    /// render the input field on stdout
    pub fn display(&self) -> Result<(), Error> {
        let mut w = std::io::stdout();
        self.display_on(&mut w)?;
        w.flush()?;
        Ok(())
    }
}

