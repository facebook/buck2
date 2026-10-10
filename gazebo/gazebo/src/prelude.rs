/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Extension methods for iterators, [`Option`] and slice/[`Vec`]. Usually imported with
//! `use gazebo::prelude::*`.

pub use crate::ext::iter::IterExactSize;
pub use crate::ext::iter::IterExt;
pub use crate::ext::iter::IterOwned;
pub use crate::ext::option::OptionExt;
pub use crate::ext::vec::SliceClonedExt;
pub use crate::ext::vec::SliceExt;
pub use crate::ext::vec::VecExt;
