/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp;

use gazebo::prelude::*;

use crate::content::LinesExt;
use crate::Line;

/// Denotes an x by y area.  Passed to [`Component`](crate::Component)s to give valid drawing area.
#[derive(Debug, Copy, Clone, Dupe, Eq, PartialEq, Default)]
pub struct Dimensions {
    pub x: usize,
    pub y: usize,
}

/// Denotes a particular axis (x or y).  Used for several Components which are generalized over direction.
#[derive(Debug, Copy, Clone, Dupe, Eq, PartialEq)]
pub enum Direction {
    Horizontal,
    Vertical,
}

impl From<(u16, u16)> for Dimensions {
    fn from((x, y): (u16, u16)) -> Self {
        Self {
            x: x.into(),
            y: y.into(),
        }
    }
}

impl From<(usize, usize)> for Dimensions {
    fn from((x, y): (usize, usize)) -> Self {
        Self { x, y }
    }
}

impl Dimensions {
    pub fn new(x: usize, y: usize) -> Self {
        Self { x, y }
    }

    pub fn dimension(self, direction: Direction) -> usize {
        match direction {
            Direction::Horizontal => self.x,
            Direction::Vertical => self.y,
        }
    }

    /// Best effort conversion - it will truncate to the nearest valid dimension.
    pub fn multiply(self, multiplicand: f64, direction: Direction) -> Self {
        fn mul(lhs: usize, rhs: f64) -> usize {
            let lhs = lhs as f64; // this might truncate the size if too large
            let result = lhs * rhs;
            result as usize // this might not be an exact dimension.
        }

        let Self { x, y } = self;
        match direction {
            Direction::Horizontal => Self {
                x: mul(x, multiplicand),
                y,
            },
            Direction::Vertical => Self {
                x,
                y: mul(y, multiplicand),
            },
        }
    }

    pub fn saturating_sub(self, subtractor: usize, direction: Direction) -> Self {
        let Self { x, y } = self;
        match direction {
            Direction::Horizontal => Self {
                x: x.saturating_sub(subtractor),
                y,
            },
            Direction::Vertical => Self {
                x,
                y: y.saturating_sub(subtractor),
            },
        }
    }

    /// Finds the size of a [`Component`](crate::Component)'s output in a given dimension.
    /// Truncates the size to at most `u16::MAX`.
    // ptr args allowed because line trait only implemented on `Vec<line>`
    #[allow(clippy::ptr_arg)]
    pub fn dimension_from_output_truncated(output: &Vec<Line>, direction: Direction) -> usize {
        match direction {
            Direction::Horizontal => output.max_line_length(),
            Direction::Vertical => output.len(),
        }
    }

    /// Returns the smallest bounding box that fits in both dimensions.
    pub fn intersect(self, Self { x, y }: Self) -> Self {
        Self {
            x: cmp::min(self.x, x),
            y: cmp::min(self.y, y),
        }
    }

    /// Returns the smallest bounding box in which both Dimensions fit.
    pub fn union(self, Self { x, y }: Self) -> Self {
        Self {
            x: cmp::max(self.x, x),
            y: cmp::max(self.y, y),
        }
    }

    /// Returns true iff the passed dimension is smaller than or equal to in both dimensions.
    pub fn contains(self, Self { x, y }: Self) -> bool {
        x <= self.x && y <= self.y
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_intersect() {
        let lhs = Dimensions { x: 5, y: 10 };
        let rhs = Dimensions { x: 8, y: 3 };
        let intersect = lhs.intersect(rhs);
        assert_eq!(intersect, Dimensions { x: 5, y: 3 });
    }

    #[test]
    fn test_union() {
        let lhs = Dimensions { x: 5, y: 10 };
        let rhs = Dimensions { x: 8, y: 3 };
        let union = lhs.union(rhs);
        assert_eq!(union, Dimensions { x: 8, y: 10 });
    }

    #[test]
    fn test_contains() {
        let lhs = Dimensions { x: 8, y: 10 };
        let rhs = Dimensions { x: 5, y: 3 };
        assert!(lhs.contains(rhs));
        assert!(!rhs.contains(lhs));
        assert!(lhs.contains(lhs));
    }
}
