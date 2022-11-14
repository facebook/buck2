/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use gazebo::prelude::IterOwned;

use crate::fs::paths::file_name::FileName;
use crate::fs::paths::file_name::FileNameBuf;
use crate::fs::paths::forward_rel_path::ForwardRelativePath;
use crate::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use crate::fs::project::ProjectRelativePath;
use crate::fs::project::ProjectRelativePathBuf;

/// Provide an iterator of FileNameBuf from inputs that can produce one. This is useful for methods
/// that insert into directory mappings.
pub trait IntoFileNameBufIterator {
    type Iterator: Iterator<Item = FileNameBuf>;

    fn into_iter(self) -> Self::Iterator;
}

impl<'a> IntoFileNameBufIterator for &'a ForwardRelativePath {
    type Iterator = impl Iterator<Item = FileNameBuf>;

    fn into_iter(self) -> Self::Iterator {
        self.iter().owned()
    }
}

impl<'a> IntoFileNameBufIterator for &'a ForwardRelativePathBuf {
    type Iterator = impl Iterator<Item = FileNameBuf>;

    fn into_iter(self) -> Self::Iterator {
        self.iter().owned()
    }
}

impl<'a> IntoFileNameBufIterator for &'a ProjectRelativePath {
    type Iterator = impl Iterator<Item = FileNameBuf>;

    fn into_iter(self) -> Self::Iterator {
        self.iter().owned()
    }
}

impl<'a> IntoFileNameBufIterator for &'a ProjectRelativePathBuf {
    type Iterator = impl Iterator<Item = FileNameBuf>;

    fn into_iter(self) -> Self::Iterator {
        self.iter().owned()
    }
}

impl<'a> IntoFileNameBufIterator for &'a FileName {
    type Iterator = impl Iterator<Item = FileNameBuf>;

    fn into_iter(self) -> Self::Iterator {
        std::iter::once(self.to_owned())
    }
}

impl<'a> IntoFileNameBufIterator for &'a FileNameBuf {
    type Iterator = impl Iterator<Item = FileNameBuf>;

    fn into_iter(self) -> Self::Iterator {
        std::iter::once(self.clone())
    }
}

impl IntoFileNameBufIterator for FileNameBuf {
    type Iterator = impl Iterator<Item = FileNameBuf>;

    fn into_iter(self) -> Self::Iterator {
        std::iter::once(self)
    }
}
