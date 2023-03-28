/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// TODO (torozco): sort out public API and remove this.
#![allow(unused)]

mod builder;
mod graph;
mod potential;
mod types;

#[cfg(test)]
mod test_utils;

pub use builder::GraphBuilder;
pub use graph::Graph;
pub use potential::compute_critical_path_potentials;
pub use types::CriticalPathIndex;
pub use types::CriticalPathVertexData;
pub use types::OptionalVertexId;
pub use types::VertexData;
pub use types::VertexId;
pub use types::VertexKeys;
