use {
    std::cmp::{Ord, PartialOrd},
};

// Implementation note: the order of fields matters here
// for derive implementation.
/// a 2D position suitable for a cursor position
/// in a text
#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Default)]
pub struct Pos {
    pub y: usize,
    pub x: usize,
}


/// A range made of two positions, both included
/// (they may be equal, in which case the range is one character long).
///
/// A Range can't be empty.
#[derive(Debug, Clone, Copy)]
pub struct Range {
    pub min: Pos,
    pub max: Pos,
}

impl Pos {
    pub const fn new(x: usize, y: usize) -> Pos {
        Self { x, y }
    }
}

impl Range {
    pub const fn contains(&self, x: usize, y: usize) -> bool {
        if self.min.y == self.max.y {
            y == self.min.y && self.min.x <= x && x <= self.max.x
        } else if y < self.min.y {
            false
        } else if y > self.max.y {
            false
        } else if y == self.min.y {
            x >= self.min.x
        } else if y == self.max.y {
            x <= self.max.x
        } else {
            true
        }
    }
    pub const fn contains_pos(&self, pos: Pos) -> bool {
        self.contains(pos.x, pos.y)
    }
}
