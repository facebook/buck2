use std::{
    cmp::Ordering,
    io::{stdout, Write},
};

use crossterm::{
    cursor::MoveTo,
    queue,
    style::{Color, SetBackgroundColor},
    terminal::{Clear, ClearType},
};

use crate::{
    compute_scrollbar, errors::Result, gray, Alignment, Area, CompoundStyle, MadSkin, Spacing,
};

pub struct ListViewCell<'t> {
    con: String,
    style: &'t CompoundStyle,
    width: usize, // length of content in chars
}

pub struct Title {
    columns: Vec<usize>, // the column(s) below this title
}

pub struct ListViewColumn<'t, T> {
    title: String,
    min_width: usize,
    max_width: usize,
    spacing: Spacing,
    extract: Box<dyn Fn(&T) -> ListViewCell<'t>>, // a function building cells from the rows
}

struct Row<T> {
    data: T,
    displayed: bool,
}

/// A filterable list whose columns can be automatically resized.
///
///
/// Notes:
/// * another version will allow more than one style per cell
/// (i.e. make the cells composites rather than compounds). Shout
/// out if you need that now.
/// * this version doesn't allow cell wrapping
pub struct ListView<'t, T> {
    titles: Vec<Title>,
    columns: Vec<ListViewColumn<'t, T>>,
    rows: Vec<Row<T>>,
    pub area: Area,
    scroll: usize,
    pub skin: &'t MadSkin,
    filter: Option<Box<dyn Fn(&T) -> bool>>, // a function determining if the row must be displayed
    displayed_rows_count: usize,
    row_order: Option<Box<dyn Fn(&T, &T) -> Ordering>>,
    selection: Option<usize>, // index of the selected line
    selection_background: Color,
}

impl<'t> ListViewCell<'t> {
    pub fn new(con: String, style: &'t CompoundStyle) -> Self {
        let width = con.chars().count();
        Self { con, style, width }
    }
}

impl<'t, T> ListViewColumn<'t, T> {
    pub fn new(
        title: &str,
        min_width: usize,
        max_width: usize,
        extract: Box<dyn Fn(&T) -> ListViewCell<'t>>,
    ) -> Self {
        Self {
            title: title.to_owned(),
            min_width,
            max_width,
            spacing: Spacing {
                width: min_width,
                align: Alignment::Center,
            },
            extract,
        }
    }
    pub const fn with_align(mut self, align: Alignment) -> Self {
        self.spacing.align = align;
        self
    }
}

impl<'t, T> ListView<'t, T> {
    /// Create a new list view with the passed columns.
    ///
    /// The columns can't be changed afterwards but the area can be modified.
    /// When two columns have the same title, those titles are merged (but
    /// the columns below stay separated).
    pub fn new(area: Area, columns: Vec<ListViewColumn<'t, T>>, skin: &'t MadSkin) -> Self {
        let mut titles: Vec<Title> = Vec::new();
        for (column_idx, column) in columns.iter().enumerate() {
            if let Some(last_title) = titles.last_mut() {
                if columns[last_title.columns[0]].title == column.title {
                    // we merge those columns titles
                    last_title.columns.push(column_idx);
                    continue;
                }
            }
            // this is a new title
            titles.push(Title {
                columns: vec![column_idx],
            });
        }
        Self {
            titles,
            columns,
            rows: Vec::new(),
            area,
            scroll: 0,
            skin,
            filter: None,
            displayed_rows_count: 0,
            row_order: None,
            selection: None,
            selection_background: gray(5),
        }
    }
    /// set a comparator for row sorting
    pub fn sort(&mut self, sort: Box<dyn Fn(&T, &T) -> Ordering>) {
        self.row_order = Some(sort);
    }
    /// return the height which is available for rows
    #[inline(always)]
    pub const fn tbody_height(&self) -> u16 {
        if self.area.height > 2 {
            self.area.height - 2
        } else {
            self.area.height
        }
    }
    /// return an option which when filled contains
    ///  a tupple with the top and bottom of the vertical
    ///  scrollbar. Return none when the content fits
    ///  the available space.
    #[inline(always)]
    pub fn scrollbar(&self) -> Option<(u16, u16)> {
        compute_scrollbar(
            self.scroll as u16,
            self.displayed_rows_count as u16,
            self.tbody_height(),
            self.area.top,
        )
    }
    pub fn add_row(&mut self, data: T) {
        let stick_to_bottom = self.row_order.is_none() && self.do_scroll_show_bottom();
        let displayed = match &self.filter {
            Some(fun) => fun(&data),
            None => true,
        };
        if displayed {
            self.displayed_rows_count += 1;
        }
        if stick_to_bottom {
            self.scroll_to_bottom();
        }
        self.rows.push(Row { data, displayed });
        if let Some(row_order) = &self.row_order {
            self.rows.sort_by(|a, b| row_order(&a.data, &b.data));
        }
    }
    /// remove all rows (and selection).
    ///
    /// Keep the columns and the sort function, if any.
    pub fn clear_rows(&mut self) {
        self.rows.clear();
        self.scroll = 0;
        self.displayed_rows_count = 0;
        self.selection = None;
    }
    /// return both the number of displayed rows and the total number
    pub fn row_counts(&self) -> (usize, usize) {
        (self.displayed_rows_count, self.rows.len())
    }
    /// recompute the widths of all columns.
    /// This should be called when the area size is modified
    pub fn update_dimensions(&mut self) {
        let available_width: i32 =
            i32::from(self.area.width)
            - (self.columns.len() as i32 - 1) // we remove the separator
            - 1; // we remove 1 to let space for the scrollbar
        let sum_min_widths: i32 = self.columns.iter().map(|c| c.min_width as i32).sum();
        if sum_min_widths >= available_width {
            for i in 0..self.columns.len() {
                self.columns[i].spacing.width = self.columns[i].min_width;
            }
        } else {
            let mut excess = available_width - sum_min_widths;
            for i in 0..self.columns.len() {
                let d =
                    ((self.columns[i].max_width - self.columns[i].min_width) as i32).min(excess);
                excess -= d;
                self.columns[i].spacing.width = self.columns[i].min_width + d as usize;
            }
            // there might be some excess, but it's better to have some space at right rather
            //  than a too wide table
        }
    }
    pub fn set_filter(&mut self, filter: Box<dyn Fn(&T) -> bool>) {
        let mut count = 0;
        for row in self.rows.iter_mut() {
            row.displayed = filter(&row.data);
            if row.displayed {
                count += 1;
            }
        }
        self.scroll = 0; // something better should be done... later
        self.displayed_rows_count = count;
        self.filter = Some(filter);
    }
    pub fn remove_filter(&mut self) {
        for row in self.rows.iter_mut() {
            row.displayed = true;
        }
        self.displayed_rows_count = self.rows.len();
        self.filter = None;
    }
    /// write the list view on the given writer
    pub fn write_on<W>(&self, w: &mut W) -> Result<()>
    where
        W: std::io::Write,
    {
        let sx = self.area.left + self.area.width;
        let vbar = self.skin.table.compound_style.style_char('│');
        let tee = self.skin.table.compound_style.style_char('┬');
        let cross = self.skin.table.compound_style.style_char('┼');
        let hbar = self.skin.table.compound_style.style_char('─');
        // title line
        queue!(w, MoveTo(self.area.left, self.area.top))?;
        for (title_idx, title) in self.titles.iter().enumerate() {
            if title_idx != 0 {
                vbar.queue(w)?;
            }
            let width = title
                .columns
                .iter()
                .map(|ci| self.columns[*ci].spacing.width)
                .sum::<usize>()
                + title.columns.len()
                - 1;
            let spacing = Spacing {
                width,
                align: Alignment::Center,
            };
            spacing.write_str(
                w,
                &self.columns[title.columns[0]].title,
                &self.skin.headers[0].compound_style,
            )?;
        }
        // separator line
        queue!(w, MoveTo(self.area.left, self.area.top + 1))?;
        for (title_idx, title) in self.titles.iter().enumerate() {
            if title_idx != 0 {
                cross.queue(w)?;
            }
            for (col_idx_idx, col_idx) in title.columns.iter().enumerate() {
                if col_idx_idx > 0 {
                    tee.queue(w)?;
                }
                for _ in 0..self.columns[*col_idx].spacing.width {
                    hbar.queue(w)?;
                }
            }
        }
        // rows, maybe scrolled
        let mut row_idx = self.scroll as usize;
        let scrollbar = self.scrollbar();
        for y in 2..self.area.height {
            queue!(w, MoveTo(self.area.left, self.area.top + y))?;
            loop {
                if row_idx == self.rows.len() {
                    queue!(w, Clear(ClearType::UntilNewLine))?;
                    break;
                }
                if self.rows[row_idx].displayed {
                    let selected = Some(row_idx) == self.selection;
                    for (col_idx, col) in self.columns.iter().enumerate() {
                        if col_idx != 0 {
                            if selected {
                                queue!(w, SetBackgroundColor(self.selection_background))?;
                            }
                            vbar.queue(w)?;
                        }
                        let cell = (col.extract)(&self.rows[row_idx].data);
                        if selected {
                            let mut style = cell.style.clone();
                            style.set_bg(self.selection_background);
                            col.spacing
                                .write_counted_str(w, &cell.con, cell.width, &style)?;
                        } else {
                            col.spacing
                                .write_counted_str(w, &cell.con, cell.width, cell.style)?;
                        }
                    }
                    row_idx += 1;
                    break;
                }
                row_idx += 1;
            }
            if let Some((sctop, scbottom)) = scrollbar {
                queue!(w, MoveTo(sx, self.area.top + y))?;
                let y = y - 2;
                if sctop <= y && y <= scbottom {
                    self.skin.scrollbar.thumb.queue(w)?;
                } else {
                    self.skin.scrollbar.track.queue(w)?;
                }
            }
        }
        Ok(())
    }
    /// display the whole list in its area
    pub fn write(&self) -> Result<()> {
        let mut stdout = stdout();
        self.write_on(&mut stdout)?;
        stdout.flush()?;
        Ok(())
    }
    /// return true if the last line of the list is visible
    pub const fn do_scroll_show_bottom(&self) -> bool {
        self.scroll + self.tbody_height() as usize >= self.displayed_rows_count
    }
    /// ensure the last line is visible
    pub fn scroll_to_bottom(&mut self) {
        let body_height = self.tbody_height() as usize;
        self.scroll = if self.displayed_rows_count > body_height {
            self.displayed_rows_count - body_height
        } else {
            0
        }
    }
    /// set the scroll amount.
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
            self.scroll = (self.scroll + lines_count as usize)
                .min(self.displayed_rows_count - self.tbody_height() as usize + 1);
        }
        self.make_selection_visible();
    }
    /// set the scroll amount.
    /// pages_count can be negative
    pub fn try_scroll_pages(&mut self, pages_count: i32) {
        self.try_scroll_lines(pages_count * self.tbody_height() as i32)
    }
    /// try to select the next visible line
    pub fn try_select_next(&mut self, up: bool) {
        if self.displayed_rows_count == 0 {
            return;
        }
        if self.displayed_rows_count == 1 || self.selection.is_none() {
            for i in 0..self.rows.len() {
                let i = (i + self.scroll as usize) % self.rows.len();
                if self.rows[i].displayed {
                    self.selection = Some(i);
                    self.make_selection_visible();
                    return;
                }
            }
        }
        for i in 0..self.rows.len() {
            let delta_idx = if up { self.rows.len() - 1 - i } else { i + 1 };
            let row_idx = (delta_idx + self.selection.unwrap()) % self.rows.len();
            if self.rows[row_idx].displayed {
                self.selection = Some(row_idx);
                self.make_selection_visible();
                return;
            }
        }
    }
    /// select the first visible line (unless there's nothing).
    pub fn select_first_line(&mut self) {
        for i in 0..self.rows.len() {
            if self.rows[i].displayed {
                self.selection = Some(i);
                self.make_selection_visible();
                return;
            }
        }
        self.selection = None;
    }
    /// select the last visible line (unless there's nothing).
    pub fn select_last_line(&mut self) {
        for i in (0..self.rows.len()).rev() {
            if self.rows[i].displayed {
                self.selection = Some(i);
                self.make_selection_visible();
                return;
            }
        }
        self.selection = None;
    }
    /// scroll to ensure the selected line (if any) is visible.
    ///
    /// This is automatically called by try_scroll
    ///  and try select functions
    pub fn make_selection_visible(&mut self) {
        let tbody_height = self.tbody_height() as usize;
        if self.displayed_rows_count <= tbody_height {
            return; // there's no scroll
        }
        if let Some(sel) = self.selection {
            if sel <= self.scroll {
                self.scroll = if sel > 2 { sel - 2 } else { 0 };
            } else if sel + 1 >= self.scroll + tbody_height {
                self.scroll = sel - tbody_height + 2;
            }
        }
    }
    pub fn get_selection(&self) -> Option<&T> {
        self.selection.map(|sel| &self.rows[sel].data)
    }
    pub const fn has_selection(&self) -> bool {
        self.selection.is_some()
    }
    pub fn unselect(&mut self) {
        self.selection = None;
    }
}
