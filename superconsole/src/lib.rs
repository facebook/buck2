/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The superconsole crate provides a handler and building blocks for powerful, yet minimally intrusive TUIs.
//! Built on-top of [`crossterm`](crossterm), it cross-compiles on Windows 7+, Linux, and MacOS.
//!
//! Rendering is handled by [`SuperConsole`](SuperConsole), which draws to [`stdout`](std::io::stdout).
//! The caller is responsible for re-rendering whenever necessary.
//! User input will cause aberrations in output; similarly, one should also not produce output from other sources while superconsole is active.
//!
//! The rendering can be divided into two principle components:
//! * In the *scratch* area, the previous content is overwritten at each render.
//! * In the *emitted* area, lines scroll away above the scratch with various diagnostic output.
//! Components live in the scratch area.
//!
//! [`State`](State) and [`Component`s](Component) are decoupled.  `Component`s are stateless, and `State` is supplied at render time.
//!
//! A set of pre-baked composition and testing oriented components are provided in the [`components`](components) module.

#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]
// We deliberately make our code stable compatible
#![cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_use_box))]

// re-exports
pub use components::Component;
pub use components::DrawMode;
pub use content::Line;
pub use content::Lines;
pub use content::Span;
pub use dimensions::Dimensions;
pub use dimensions::Direction;
pub use error::Error;
pub use state::State;

pub use crate::builder::Builder;
pub use crate::superconsole::SuperConsole;

pub mod builder;
pub mod components;
pub mod content;
mod dimensions;
mod error;
mod output;
mod state;
pub mod style;
mod superconsole;
pub mod testing;
