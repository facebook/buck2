/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Provides stylization for Strings.
//! - Create a styled string using `style`.
//! - Set the foreground or background color of the string using the `Color` enum.
//! - Set the attribute (bold, italic, underlined, etc) using the `Attribute` enum.

/// Re-export crossterm ideas about stylization to users of `superconsole`.
pub use crossterm::style::{style, Attribute, Color, ContentStyle, StyledContent, Stylize};
