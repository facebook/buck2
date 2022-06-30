/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod map {
    pub use starlark_map::small_map::Entry;
    pub use starlark_map::small_map::OccupiedEntry;
    pub use starlark_map::small_map::SmallMap;
    pub use starlark_map::small_map::VacantEntry;
}

pub mod set {
    pub use starlark_map::small_set::SmallSet;
}

pub use starlark_map::Equivalent;
