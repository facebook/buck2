/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Provides stylization for Strings.
//! - Create a styled string using `style`.
//! - Set the foreground or background color of the string using the `Color` enum.
//! - Set the attribute (bold, italic, underlined, etc) using the `Attribute` enum.

pub use crossterm::style::Attribute;
pub use crossterm::style::Color;
pub use crossterm::style::ContentStyle;
pub use crossterm::style::StyledContent;
pub use crossterm::style::Stylize;
pub use crossterm::style::style;
