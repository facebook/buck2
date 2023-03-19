/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A 'Configuration' is a set of attributes that are attached to each node in
//! the 'static graph' that affects the behaviour of the build. Examples of
//! these attributes are the target platform, and compiler settings.
//!
//! 'Configuration's are propagated from the top level request node to each of
//! the transitive child nodes. During propagation, the configuration may change
//! under a "transition". Multiple distinct configurations may be applied to the
//! transitive graph, effectively duplicating the graph to create two distinct
//! graphs with different build behaviours (split-transitions).
//!

pub mod cfg_diff;
pub mod constraints;
pub mod data;
pub mod hash;
pub mod pair;
pub mod transition;
