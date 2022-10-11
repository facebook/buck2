use {
    super::{Pos, Range},
    crate::TAB_REPLACEMENT,
    std::{
        fmt,
    },
    unicode_width::UnicodeWidthChar,
};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Line {
    pub chars: Vec<char>,
}
/// an iterator over the chars of an InputFieldContent or
/// of a selection
pub struct Chars<'c> {
    content: &'c InputFieldContent,
    pos: Pos,
    end: Pos,
}
/// the content of an InputField.
///
/// Doesn't know about rendering, styles, areas, etc.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InputFieldContent {
    /// the cursor's position
    pos: Pos,
    /// the other end of the selection, if any
    selection_tail: Option<Pos>,
    /// text lines, always at least one
    lines: Vec<Line>,
}

impl Iterator for Chars<'_> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        if self.pos > self.end {
            return None;
        }
        let line = &self.content.lines[self.pos.y];
        if self.pos.x < line.chars.len() {
            self.pos.x += 1;
            Some(line.chars[self.pos.x - 1])
        } else if self.pos.y + 1 < self.content.lines.len() {
            self.pos.y += 1;
            self.pos.x = 0;
            Some('\n')
        } else {
            None
        }
    }
}
impl<'c> IntoIterator for &'c InputFieldContent {
    type Item = char;
    type IntoIter = Chars<'c>;
    fn into_iter(self) -> Self::IntoIter {
        Chars {
            content: self,
            pos: Pos::default(),
            end: self.end(),
        }
    }
}

impl fmt::Display for Line {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use fmt::Write;
        for &c in &self.chars {
            f.write_char(c)?;
        }
        Ok(())
    }
}

impl Default for InputFieldContent {
    fn default() -> Self {
        Self {
            pos: Pos::default(),
            selection_tail: None,
            // there's always a line
            lines: vec![Line::default()],
        }
    }
}

impl fmt::Display for InputFieldContent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use fmt::Write;
        let mut lines = self.lines.iter().peekable();
        loop {
            if let Some(line) = lines.next() {
                for &c in &line.chars {
                    f.write_char(c)?;
                }
                if lines.peek().is_some() {
                    f.write_char('\n')?;
                }
            } else {
                break;
            }
        }
        Ok(())
    }
}

impl Line {
    pub fn col_to_char_idx(&self, col: usize) -> Option<usize> {
        let mut sum_widths = 0;
        for (idx, &c) in self.chars.iter().enumerate() {
            if col <= sum_widths {
                return Some(idx);
            }
            sum_widths += InputFieldContent::char_width(c);
        }
        None
    }
    pub fn char_idx_to_col(&self, idx: usize) -> usize {
        self.chars[0..idx].iter()
            .map(|&c| InputFieldContent::char_width(c))
            .sum()
    }
    pub fn width(&self) -> usize {
        self.chars.iter().map(|&c| InputFieldContent::char_width(c)).sum()
    }
}

impl InputFieldContent {
    pub fn line_count(&self) -> usize {
        self.lines.len()
    }
    pub fn line(&self, y: usize) -> Option<&Line> {
        self.lines.get(y)
    }
    pub fn line_saturating(&self, y: usize) -> &Line {
        self.lines.get(y)
            .unwrap_or(&self.lines[self.lines.len()-1])
    }
    pub fn current_line(&self) -> &Line {
        self.lines
            .get(self.pos.y)
            .expect("current line should exist")
    }
    pub fn lines(&self) -> &[Line] {
        &self.lines
    }
    pub const fn cursor_pos(&self) -> Pos {
        self.pos
    }
    /// Set the cursor position.
    ///
    /// The position set may be different to ensure consistency
    /// (for example if it's after the end, it will be set back).
    pub fn set_cursor_pos(&mut self, new_pos: Pos) {
        if new_pos.y >= self.lines.len() {
            self.pos = self.end();
        } else {
            self.pos.y = new_pos.y;
            self.pos.x = new_pos.x.min(self.lines[self.pos.y].chars.len());
        }
    }
    /// Set the selection tail to the current pos if there's no selection
    pub fn make_selection(&mut self) {
        if self.selection_tail.is_none() {
            self.selection_tail = Some(self.pos);
        }
    }
    pub fn unselect(&mut self) {
        self.selection_tail = None;
    }
    pub fn set_selection_tail(&mut self, sel_tail: Pos) {
        if sel_tail.y >= self.lines.len() {
            self.selection_tail = Some(self.end());
        } else {
            self.selection_tail = Some(Pos {
                y: sel_tail.y,
                x: sel_tail.x.min(self.lines[self.pos.y].chars.len()),
            });
        }
    }
    pub fn selection(&self) -> Range {
        if let Some(sel_tail) = self.selection_tail {
            if sel_tail < self.pos {
                Range { min: sel_tail, max: self.pos }
            } else {
                Range { min: self.pos, max: sel_tail }
            }
        } else {
            Range { min: self.pos, max: self.pos }
        }
    }
    /// return an iterator over the characters of the
    /// selection (including some newline chars maybe)
    pub fn selection_chars(&self) -> Chars<'_> {
        let Range { min, max } = self.selection();
        Chars {
            content: self,
            pos: min,
            end: max,
        }
    }
    pub fn selection_string(&self) -> String {
        self.selection_chars().collect()
    }
    pub fn is_empty(&self) -> bool {
        match self.lines.len() {
            1 => self.lines[0].chars.is_empty(),
            _ => false,
        }
    }
    pub const fn has_selection(&self) -> bool {
        self.selection_tail.is_some()
    }
    pub fn has_wide_selection(&self) -> bool {
        self.selection_tail.map_or(false, |sel_tail| sel_tail != self.pos)
    }
    /// return the position on end, where the cursor should be put
    /// initially
    pub fn end(&self) -> Pos {
        let y = self.lines.len() - 1;
        Pos { x:self.lines[y].chars.len(), y }
    }
    fn last_line(&mut self) -> &mut Line {
        let y = self.lines.len() - 1;
        &mut self.lines[y]
    }
    /// add a char at end, without updating the position.
    ///
    /// This shouldn't be used in normal event handling as
    /// characters are normally inserted on insertion point
    /// with insert_char.
    pub fn push_char(&mut self, c: char) {
        if c == '\n' {
            self.lines.push(Line::default());
        } else if c == '\r' {
            // do nothing, it probably comes from a copy-paste on windows
        } else if c == '\x08' { // backspace
            // do nothing, we don't want those in our inputfield
        } else {
            self.last_line().chars.push(c);
        }
    }
    /// Initialize from a string, with the cursor at end
    pub fn from<S: AsRef<str>>(s: S) -> Self {
        let mut content = Self::default();
        content.insert_str(s);
        content
    }
    pub fn clear(&mut self) {
        self.lines.clear();
        self.lines.push(Line::default());
        self.pos = Pos::default();
    }
    pub fn insert_new_line(&mut self) {
        let new_line = Line {
            chars: self.lines[self.pos.y].chars.split_off(self.pos.x),
        };
        self.pos.x = 0;
        self.pos.y += 1;
        self.lines.insert(self.pos.y, new_line);
    }
    /// Insert a character at the current position, updating
    /// this position
    pub fn insert_char(&mut self, c: char) {
        if c == '\n' {
            self.insert_new_line();
        } else if c == '\r' || c == '\x08' {
            // skipping
        } else {
            self.lines[self.pos.y].chars.insert(self.pos.x, c);
            self.pos.x += 1;
        }
    }
    /// Insert the string on cursor point, as if it was typed
    pub fn insert_str<S: AsRef<str>>(&mut self, s: S) {
        for c in s.as_ref().chars() {
            self.insert_char(c);
        }
    }
    /// Tell whether the content of the input is equal to the argument,
    /// comparing char by char
    pub fn is_str(&self, s: &str) -> bool {
        let mut ia = self.into_iter();
        let mut ib = s.chars();
        loop {
            match (ia.next(), ib.next()) {
                (Some(a), Some(b)) if a == b => { continue }
                (None, None) => { return true; }
                _ => { return false; }
            }
        }
    }
    /// Change the content to the new one and put the cursor at the end **if** the
    ///  content is different from the previous one.
    ///
    ///  Don't move the cursor if the string content didn't change.
    pub fn set_str<S: AsRef<str>>(&mut self, s: S) {
        if self.is_str(s.as_ref()) {
            return;
        }
        self.clear();
        self.insert_str(s);
    }
    /// Remove the char left of the cursor, if any.
    pub fn del_char_left(&mut self) -> bool {
        if self.pos.x > 0 {
            self.pos.x -= 1;
            if !self.lines[self.pos.y].chars.is_empty() {
                self.lines[self.pos.y].chars.remove(self.pos.x);
            }
            true
        } else if self.pos.y > 0 && self.lines.len() > 1 {
            let mut removed_line = self.lines.remove(self.pos.y);
            self.pos.y -= 1;
            self.pos.x = self.lines[self.pos.y].chars.len();
            self.lines[self.pos.y].chars.append(&mut removed_line.chars);
            true
        } else {
            false
        }
    }

    /// make the word around the current pos, if any, the current selection
    pub fn select_word_around(&mut self) -> bool {
        let chars = &self.lines[self.pos.y].chars;
        let mut start = self.pos.x;
        if start >= chars.len() || !is_word_char(chars[start]) {
            return false;
        }
        while start > 0 && is_word_char(chars[start-1]) {
            start -= 1;
        }
        let mut end = self.pos.x;
        while end + 1 < chars.len() && is_word_char(chars[end+1]) {
            end += 1;
        }
        self.selection_tail = Some(Pos::new(start, self.pos.y));
        self.pos.x = end;
        true
    }

    /// Remove the char at cursor position, if any.
    ///
    /// Cursor position is unchanged
    pub fn del_char_below(&mut self) -> bool {
        let line_len = self.current_line().chars.len();
        if line_len == 0 {
            if self.lines.len() > 1 {
                self.lines.remove(self.pos.y);
                true
            } else {
                false
            }
        } else if self.pos.x < line_len {
            self.lines[self.pos.y].chars.remove(self.pos.x);
            true
        } else if self.lines.len() > self.pos.y + 1 {
            let mut removed_line = self.lines.remove(self.pos.y + 1);
            self.lines[self.pos.y].chars.append(&mut removed_line.chars);
            true
        } else {
            false
        }
    }

    pub fn del_selection(&mut self) -> bool {
        let Range { min, max } = self.selection();
        if min.y == max.y {
            if min.x == max.x {
                return self.del_char_below();
            }
            if max.x == self.lines[min.y].chars.len() {
                if min.x == 0 {
                    // we remove the whole line
                    self.lines.drain(min.y..min.y+1);
                    if self.lines.is_empty() {
                        self.lines.push(Line::default());
                    }
                } else {
                    self.lines[min.y].chars.drain(min.x..);
                }
            } else {
                self.lines[min.y].chars.drain(min.x..max.x+1);
            }
        } else {
            let min_y = if min.x > 0 {
                self.lines[min.y].chars.truncate(min.x);
                min.y + 1
            } else {
                min.y
            };
            let max_y = if max.x < self.lines[max.y].chars.len() {
                self.lines[max.y].chars.drain(0..max.x);
                max.y - 1
            } else {
                max.y
            };
            if max_y > min_y {
                self.lines.drain(min_y..(max_y+1).min(self.lines.len()));
                if self.lines.is_empty() {
                    self.lines.push(Line::default());
                }
            }
        }
        self.set_cursor_pos(min);
        self.selection_tail = None;
        true
    }
    /// Swap two lines. Return false if one of the indices is out of
    /// range or if the two indices are the same
    pub fn swap_lines(&mut self, ya: usize, yb: usize) -> bool {
        if ya != yb && ya < self.lines.len() && yb < self.lines.len() {
            self.lines.swap(ya, yb);
            true
        } else {
            false
        }
    }

    /// Swap the current line with the line before, if possible
    pub fn move_current_line_up(&mut self) -> bool {
        if self.pos.y > 0 {
            if self.swap_lines(self.pos.y - 1, self.pos.y) {
                self.pos.y -= 1;
                return true;
            }
        }
        false
    }

    /// Swap the current line with the line after, if possible
    pub fn move_current_line_down(&mut self) -> bool {
        if self.swap_lines(self.pos.y + 1, self.pos.y) {
            self.pos.y += 1;
            true
        } else {
            false
        }
    }

    /// Move the cursor to the right (or to the line below
    /// if it's a the end of a non-last line)
    pub fn move_right(&mut self) -> bool {
        if self.pos.x < self.lines[self.pos.y].chars.len() {
            self.pos.x += 1;
            true
        } else if self.pos.y < self.lines.len() - 1 {
            self.pos.y += 1;
            self.pos.x = 0;
            true
        } else {
            false
        }
    }
    /// Move the cursor up
    pub fn move_lines_up(&mut self, lines: usize) -> bool {
        if self.pos.y > 0 {
            let cols = self.lines[self.pos.y].char_idx_to_col(self.pos.x);
            self.pos.y -= lines.min(self.pos.y);
            let line = &self.lines[self.pos.y];
            self.pos.x = line.col_to_char_idx(cols).unwrap_or(line.chars.len());
            true
        } else {
            false
        }
    }
    /// Move the cursor one line up
    pub fn move_up(&mut self) -> bool {
        self.move_lines_up(1)
    }
    /// Move the cursor down
    pub fn move_lines_down(&mut self, lines: usize) -> bool {
        if self.pos.y + 1 < self.lines.len() {
            let cols = self.lines[self.pos.y].char_idx_to_col(self.pos.x);
            self.pos.y += lines.min(self.lines.len() - self.pos.y - 1);
            let line = &self.lines[self.pos.y];
            self.pos.x = line.col_to_char_idx(cols).unwrap_or(line.chars.len());
            true
        } else {
            false
        }
    }
    pub fn move_down(&mut self) -> bool {
        self.move_lines_down(1)
    }
    pub fn move_left(&mut self) -> bool {
        if self.pos.x > 0 {
            self.pos.x -= 1;
            true
        } else if self.pos.y > 0 {
            self.pos.y -= 1;
            self.pos.x = self.lines[self.pos.y].chars.len();
            true
        } else {
            false
        }
    }
    pub fn move_to_end(&mut self) -> bool {
        let pos = self.end();
        if pos == self.pos {
            false
        } else {
            self.pos = pos;
            true
        }
    }
    pub fn move_to_start(&mut self) -> bool {
        let pos = Pos { x: 0, y: 0 };
        if pos == self.pos {
            false
        } else {
            self.pos = pos;
            true
        }
    }
    pub fn move_to_line_end(&mut self) -> bool {
        let line_len = self.lines[self.pos.y].chars.len();
        if self.pos.x < line_len {
            self.pos.x = line_len;
            true
        } else {
            false
        }
    }
    pub fn move_to_line_start(&mut self) -> bool {
        if self.pos.x > 0 {
            self.pos.x = 0;
            true
        } else {
            false
        }
    }
    pub fn move_word_left(&mut self) -> bool {
        if self.pos.x > 0 {
            let chars = &self.lines[self.pos.y].chars;
            loop {
                self.pos.x -= 1;
                if self.pos.x == 0 || !chars[self.pos.x-1].is_alphanumeric() {
                    break;
                }
            }
            true
        } else {
            false
        }
    }
    pub fn move_word_right(&mut self) -> bool {
        if self.pos.x < self.lines[self.pos.y].chars.len() {
            let chars = &self.lines[self.pos.y].chars;
            loop {
                self.pos.x += 1;
                if self.pos.x +1 >= chars.len() || !chars[self.pos.x+1].is_alphanumeric() {
                    break;
                }
            }
            true
        } else {
            false
        }
    }
    pub fn del_word_left(&mut self) -> bool {
        if self.pos.x > 0 {
            let chars = &mut self.lines[self.pos.y].chars;
            loop {
                self.pos.x -= 1;
                chars.remove(self.pos.x);
                if self.pos.x == 0 || !chars[self.pos.x-1].is_alphanumeric() {
                    break;
                }
            }
            true
        } else {
            false
        }
    }
    /// Delete the word rigth of the cursor.
    ///
    // I'm not yet sure of what should be the right behavior but all changes
    // should be discussed from cases defined as in the unit tests below
    pub fn del_word_right(&mut self) -> bool {
        let chars = &mut self.lines[self.pos.y].chars;
        if self.pos.x < chars.len() {
            loop {
                let deleted_is_an = chars[self.pos.x].is_alphanumeric();
                chars.remove(self.pos.x);
                if !deleted_is_an {
                    break;
                }
                if self.pos.x == chars.len() {
                    if self.pos.x > 0 {
                        self.pos.x -= 1;
                    }
                    break;
                }
            }
            true
        } else if self.pos.x == self.current_line().chars.len() && self.pos.x > 0 {
            self.pos.x -= 1;
            true
        } else {
            false
        }
    }

    /// Return the number of columns taken by a char. It's
    /// assumed the char isn't '\r', `\n', or backspace
    /// (none of those can be in the inputfield lines)
    pub fn char_width(c: char) -> usize {
        match c {
            '\t' => TAB_REPLACEMENT.len(),
            _ => UnicodeWidthChar::width(c).unwrap_or(0),
        }
    }

}

#[test]
fn test_char_iterator() {
    let texts = vec![
        "this has\nthree lines\n",
        "",
        "123",
        "\n\n",
    ];
    for text in texts {
        assert!(InputFieldContent::from(text).is_str(text));
    }
}

#[cfg(test)]
mod input_content_edit_monoline_tests {

    use super::*;

    /// make an input for tests from two strings:
    /// - the content string (no wide chars)
    /// - a cursor position specified as a string with a caret
    fn make_content(value: &str, cursor_pos: &str) -> InputFieldContent {
        let mut content = InputFieldContent::from(value);
        content.pos = Pos {
            x: cursor_pos.chars().position(|c| c=='^').unwrap(),
            y: 0,
        };
        content
    }

    fn check(a: &InputFieldContent, value: &str, cursor_pos: &str) {
        let b = make_content(value, cursor_pos);
        assert_eq!(a, &b);
    }

    /// test the behavior of new line insertion
    #[test]
    fn test_new_line() {
        let mut con = make_content(
            "12345",
            "  ^  "
        );
        con.insert_char('6');
        check(
            &con,
            "126345",
            "   ^  ",
        );
        con.insert_new_line();
        assert!(con.is_str("126\n345"));
        let mut con = InputFieldContent::default();
        con.insert_char('1');
        con.insert_char('2');
        con.insert_new_line();
        con.insert_char('3');
        con.insert_char('4');
        assert!(con.is_str("12\n34"));
    }

    /// test the behavior of del_word_right
    #[test]
    fn test_del_word_right() {
        let mut con = make_content(
            "aaa bbb ccc",
            "     ^     ",
        );
        con.del_word_right();
        check(
            &con,
            "aaa bccc",
            "     ^  ",
        );
        con.del_word_right();
        check(
            &con,
            "aaa b",
            "    ^",
        );
        con.del_word_right();
        check(
            &con,
            "aaa ",
            "   ^",
        );
        con.del_word_right();
        check(
            &con,
            "aaa",
            "   ^",
        );
        con.del_word_right();
        check(
            &con,
            "aaa",
            "  ^",
        );
        con.del_word_right();
        check(
            &con,
            "aa",
            " ^",
        );
        con.del_word_right();
        check(
            &con,
            "a",
            "^",
        );
        con.del_word_right();
        check(
            &con,
            "",
            "^",
        );
        con.del_word_right();
        check(
            &con,
            "",
            "^",
        );
    }
}


fn is_word_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}
