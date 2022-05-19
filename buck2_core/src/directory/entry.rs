/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use derive_more::Display;
use gazebo::{prelude::*, variants::UnpackVariants};

/// An entry in a Directory, parameterized by the type of children directories and the type of leaf
/// nodes. We expect to be able to traverse directories, and we don't traverse leaves.
#[derive(Clone, Dupe, Debug, Eq, PartialEq, Display, Hash, UnpackVariants)]
#[display(bound = "D: ::std::fmt::Display, L: ::std::fmt::Display")]
pub enum DirectoryEntry<D, L> {
    Dir(D),
    Leaf(L),
}

impl<D, L> DirectoryEntry<D, L> {
    pub fn map_dir<U>(self, f: impl FnOnce(D) -> U) -> DirectoryEntry<U, L> {
        match self {
            Self::Dir(d) => DirectoryEntry::Dir(f(d)),
            Self::Leaf(l) => DirectoryEntry::Leaf(l),
        }
    }

    pub fn map_leaf<U>(self, f: impl FnOnce(L) -> U) -> DirectoryEntry<D, U> {
        match self {
            Self::Dir(d) => DirectoryEntry::Dir(d),
            Self::Leaf(l) => DirectoryEntry::Leaf(f(l)),
        }
    }

    pub fn as_ref(&self) -> DirectoryEntry<&'_ D, &'_ L> {
        match self {
            Self::Dir(ref d) => DirectoryEntry::Dir(d),
            Self::Leaf(ref l) => DirectoryEntry::Leaf(l),
        }
    }
}
