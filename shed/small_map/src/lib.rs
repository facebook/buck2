/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod map {
    pub use starlark::collections::small_map::Entry;
    pub use starlark::collections::small_map::OccupiedEntry;
    pub use starlark::collections::small_map::VacantEntry;
    pub use starlark::collections::SmallMap;
}

pub mod set {
    pub use starlark::collections::SmallSet;
}
