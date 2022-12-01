/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp;

use crate::content::LinesExt;
use crate::Line;

/// Denotes a rectangular area.
///
/// Passed to [`Component`](crate::Component)s to give valid drawing area.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub struct Dimensions {
    pub width: usize,
    pub height: usize,
}

/// Denotes a particular axis (x or y).  Used for several Components which are generalized over direction.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Direction {
    Horizontal,
    Vertical,
}

impl From<(u16, u16)> for Dimensions {
    fn from((x, y): (u16, u16)) -> Self {
        Self {
            width: x.into(),
            height: y.into(),
        }
    }
}

impl From<(usize, usize)> for Dimensions {
    fn from((width, height): (usize, usize)) -> Self {
        Self { width, height }
    }
}

impl Dimensions {
    pub fn new(width: usize, height: usize) -> Self {
        Self { width, height }
    }

    pub fn dimension(self, direction: Direction) -> usize {
        match direction {
            Direction::Horizontal => self.width,
            Direction::Vertical => self.height,
        }
    }

    /// Best effort conversion - it will truncate to the nearest valid dimension.
    pub fn multiply(self, multiplicand: f64, direction: Direction) -> Self {
        fn mul(lhs: usize, rhs: f64) -> usize {
            let lhs = lhs as f64; // this might truncate the size if too large
            let result = lhs * rhs;
            result as usize // this might not be an exact dimension.
        }

        let Self { width, height } = self;
        match direction {
            Direction::Horizontal => Self {
                width: mul(width, multiplicand),
                height,
            },
            Direction::Vertical => Self {
                width,
                height: mul(height, multiplicand),
            },
        }
    }

    pub fn saturating_sub(self, subtractor: usize, direction: Direction) -> Self {
        let Self { width, height } = self;
        match direction {
            Direction::Horizontal => Self {
                width: width.saturating_sub(subtractor),
                height,
            },
            Direction::Vertical => Self {
                width,
                height: height.saturating_sub(subtractor),
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
    pub fn intersect(self, Self { width, height }: Self) -> Self {
        Self {
            width: cmp::min(self.width, width),
            height: cmp::min(self.height, height),
        }
    }

    /// Returns the smallest bounding box in which both Dimensions fit.
    pub fn union(self, Self { width, height }: Self) -> Self {
        Self {
            width: cmp::max(self.width, width),
            height: cmp::max(self.height, height),
        }
    }

    /// Returns true iff the passed dimension is smaller than or equal to in both dimensions.
    pub fn contains(self, Self { width, height }: Self) -> bool {
        width <= self.width && height <= self.height
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_intersect() {
        let lhs = Dimensions {
            width: 5,
            height: 10,
        };
        let rhs = Dimensions {
            width: 8,
            height: 3,
        };
        let intersect = lhs.intersect(rhs);
        assert_eq!(
            intersect,
            Dimensions {
                width: 5,
                height: 3
            }
        );
    }

    #[test]
    fn test_union() {
        let lhs = Dimensions {
            width: 5,
            height: 10,
        };
        let rhs = Dimensions {
            width: 8,
            height: 3,
        };
        let union = lhs.union(rhs);
        assert_eq!(
            union,
            Dimensions {
                width: 8,
                height: 10
            }
        );
    }

    #[test]
    fn test_contains() {
        let lhs = Dimensions {
            width: 8,
            height: 10,
        };
        let rhs = Dimensions {
            width: 5,
            height: 3,
        };
        assert!(lhs.contains(rhs));
        assert!(!rhs.contains(lhs));
        assert!(lhs.contains(lhs));
    }
}
