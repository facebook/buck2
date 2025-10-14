/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Borrow;
use std::ops::Deref;

use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use ref_cast::RefCast;

/// A path identifying a cgroup.
///
/// This path includes the cgroup fs mount point, typically `/sys/fs/cgroup`.
#[derive(
    derive_more::Display,
    Debug,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    ref_cast::RefCast
)]
#[display("{}", _0.display())]
#[repr(transparent)]
pub struct CgroupPath(AbsNormPath);

impl CgroupPath {
    pub fn new<'a>(path: &'a AbsNormPath) -> &'a Self {
        Self::ref_cast(path)
    }

    pub fn to_buf(&self) -> CgroupPathBuf {
        CgroupPathBuf(self.0.to_buf())
    }

    pub fn join(&self, path: &ForwardRelativePath) -> CgroupPathBuf {
        CgroupPathBuf(self.0.join(path))
    }
}

/// Owned version of [`CgroupPath`].
#[derive(
    derive_more::Display,
    Debug,
    Clone,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord
)]
#[display("{}", _0.display())]
pub struct CgroupPathBuf(AbsNormPathBuf);

impl CgroupPathBuf {
    pub fn new(path: AbsNormPathBuf) -> Self {
        Self(path)
    }
}

impl Deref for CgroupPath {
    type Target = AbsNormPath;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Deref for CgroupPathBuf {
    type Target = CgroupPath;

    fn deref(&self) -> &Self::Target {
        RefCast::ref_cast(&*self.0)
    }
}

impl Borrow<CgroupPath> for CgroupPathBuf {
    fn borrow(&self) -> &CgroupPath {
        self
    }
}
