/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Provides a variety of utilities for working with [`Line`s](Line).
//! In order to work with [`Component`](crate::Component) output, one must import [`LinesExt`](LinesExt)

pub use line::Line;
pub use lines::{
    colored_lines_from_multiline_string, lines_from_multiline_string, Lines, LinesExt,
};
pub use span::Span;

mod line;
mod lines;
mod span;
