use {
    crossterm::terminal,
    std::convert::{TryFrom, TryInto},
};

/// A default width which is used when we failed measuring the real terminal width
const DEFAULT_TERMINAL_WIDTH: u16 = 50;

/// A default height which is used when we failed measuring the real terminal width
const DEFAULT_TERMINAL_HEIGHT: u16 = 20;

pub trait AreaContent {
    fn height() -> u16;
}

/// A rectangular part of the screen
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Area {
    pub left: u16,
    pub top: u16,
    pub width: u16,
    pub height: u16,
}

impl Default for Area {
    fn default() -> Self {
        Self::uninitialized()
    }
}

impl Area {
    /// build a new area. You'll need to set the position and size
    /// before you can use it
    pub const fn uninitialized() -> Area {
        Area {
            left: 0,
            top: 0,
            height: 1,
            width: 5,
        }
    }

    /// build a new area.
    pub const fn new(left: u16, top: u16, width: u16, height: u16) -> Area {
        Area {
            left,
            top,
            width,
            height,
        }
    }

    /// build an area covering the whole terminal
    pub fn full_screen() -> Area {
        let (width, height) = terminal_size();
        Area {
            left: 0,
            top: 0,
            width,
            height,
        }
    }

    pub const fn right(&self) -> u16 {
        self.left + self.width
    }

    pub const fn bottom(&self) -> u16 {
        self.top + self.height
    }

    /// tell whether the char at (x,y) is in the area
    pub const fn contains(&self, x: u16, y: u16) -> bool {
        x >= self.left
            && x < self.left + self.width
            && y >= self.top
            && y < self.top + self.height
    }

    /// shrink the area
    pub fn pad(&mut self, dx: u16, dy: u16) {
        // this will crash if padding is too big. feature?
        self.left += dx;
        self.top += dy;
        self.width -= 2 * dx;
        self.height -= 2 * dy;
    }

    /// symmetrically shrink the area if its width is bigger than `max_width`
    pub fn pad_for_max_width(&mut self, max_width: u16) {
        if max_width >= self.width {
            return;
        }
        let pw = self.width - max_width;
        self.left += pw / 2;
        self.width -= pw;
    }

    /// Return an option which when filled contains
    ///  a tupple with the top and bottom of the vertical
    ///  scrollbar. Return none when the content fits
    ///  the available space.
    pub fn scrollbar<U>(
        &self,
        scroll: U, // number of lines hidden on top
        content_height: U,
    ) -> Option<(u16, u16)>
    where U: Into<usize>
    {
        compute_scrollbar(scroll, content_height, self.height, self.top)
    }
}

/// Compute the min and max y (from the top of the terminal, both inclusive)
/// for the thumb part of the scrollbar which would represent the scrolled
/// content in the available height.
///
/// If you represent some data in an Area, you should directly use the
/// scrollbar method of Area.
pub fn compute_scrollbar<U1, U2, U3>(
    scroll: U1,           // 0 for no scroll, positive if scrolled
    content_height: U1,   // number of lines of the content
    available_height: U3, // for an area it's usually its height
    top: U2,              // distance from the top of the screen
) -> Option<(U2, U2)>
where
    U1: Into<usize>, // the type in which you store your content length and content scroll
    U2: Into<usize> + TryFrom<usize>, // the drawing type (u16 for an area)
    <U2 as TryFrom<usize>>::Error: std::fmt::Debug,
    U3: Into<usize> + TryFrom<usize>, // the type used for available height
    <U3 as TryFrom<usize>>::Error: std::fmt::Debug,
{
    let scroll: usize = scroll.into();
    let content_height: usize = content_height.into();
    let available_height: usize = available_height.into();
    let top: usize = top.into();
    if content_height <= available_height {
        return None;
    }
    let mut track_before = scroll * available_height / content_height;
    if track_before == 0 && scroll > 0 {
        track_before = 1;
    }
    let thumb_height = available_height * available_height / content_height;
    let scrollbar_top = top + track_before;
    let mut scrollbar_bottom = scrollbar_top + thumb_height;
    if scroll + available_height < content_height && available_height > 3 {
        scrollbar_bottom = scrollbar_bottom
            .min(top + available_height - 2)
            .max(scrollbar_top);
    }
    // by construction those two conversions are OK
    // (or it's a bug, which, well, is possible...)
    let scrollbar_top = scrollbar_top.try_into().unwrap();
    let scrollbar_bottom = scrollbar_bottom.try_into().unwrap();
    Some((scrollbar_top, scrollbar_bottom))
}

/// Return a (width, height) with the dimensions of the available
/// terminal in characters.
///
pub fn terminal_size() -> (u16, u16) {
    let size = terminal::size();
    size.unwrap_or((DEFAULT_TERMINAL_WIDTH, DEFAULT_TERMINAL_HEIGHT))
}
