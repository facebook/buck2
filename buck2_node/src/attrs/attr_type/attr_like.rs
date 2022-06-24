/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

pub trait AttrLike: Display + Debug + Clone + Eq + PartialEq + Hash + Send + Sync {}

impl<T: Display + Debug + Clone + Eq + Hash + Send + Sync + PartialEq> AttrLike for T {}
