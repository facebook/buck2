/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub(crate) mod base;
mod fat;
mod slice;
mod string_like;
mod thin;

pub use crate::arc_str::fat::ArcStr;
pub use crate::arc_str::slice::ArcSlice;
pub use crate::arc_str::string_like::ArcS;
pub use crate::arc_str::string_like::StringInside;
pub use crate::arc_str::thin::ThinArcStr;
