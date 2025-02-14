/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;

use crate::materializers::deferred::data_tree::DataTree;

pub type FileTree<V> = DataTree<FileNameBuf, V>;

struct NoopCollector;

impl<'a> FromIterator<&'a FileNameBuf> for NoopCollector {
    fn from_iter<I>(_iter: I) -> Self
    where
        I: IntoIterator<Item = &'a FileNameBuf>,
    {
        NoopCollector
    }
}

impl<V: 'static> FileTree<V> {
    pub fn iter_with_paths(&self) -> impl Iterator<Item = (ForwardRelativePathBuf, &V)> {
        self.iter::<ForwardRelativePathBuf>()
    }

    pub fn iter_without_paths(&self) -> impl Iterator<Item = &V> {
        self.iter::<NoopCollector>().map(|(NoopCollector, v)| v)
    }

    pub fn into_iter_with_paths(self) -> impl Iterator<Item = (ForwardRelativePathBuf, V)> {
        self.into_iter::<ForwardRelativePathBuf>()
    }

    #[allow(unused)]
    pub fn into_iter_without_paths(self) -> impl Iterator<Item = V> {
        self.into_iter::<NoopCollector>()
            .map(|(NoopCollector, v)| v)
    }
}
