/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! General macros useful for path declaration

use std::cmp;

/// Generates ['cmp::PartialEq'] and ['cmp::PartialOrd'] for the `lhs` and `rhs`
/// types, where `ty` is the unowned, reference path type.
pub macro impl_cmp($lhs:ty, $rhs:ty, $ty:ty) {
    impl cmp::PartialEq<$rhs> for $lhs {
        #[inline]
        fn eq(&self, other: &$rhs) -> bool {
            <$ty as cmp::PartialEq>::eq(self, other)
        }
    }

    impl cmp::PartialEq<$lhs> for $rhs {
        #[inline]
        fn eq(&self, other: &$lhs) -> bool {
            <$ty as cmp::PartialEq>::eq(self, other)
        }
    }

    impl cmp::PartialOrd<$rhs> for $lhs {
        #[inline]
        fn partial_cmp(&self, other: &$rhs) -> Option<cmp::Ordering> {
            <$ty as cmp::PartialOrd>::partial_cmp(self, other)
        }
    }

    impl cmp::PartialOrd<$lhs> for $rhs {
        #[inline]
        fn partial_cmp(&self, other: &$lhs) -> Option<cmp::Ordering> {
            <$ty as cmp::PartialOrd>::partial_cmp(self, other)
        }
    }
}

#[inline]
pub fn eq_as_ref(a: impl AsRef<str>, b: impl AsRef<str>) -> bool {
    a.as_ref() == b.as_ref()
}

#[inline]
pub fn partial_cmp_as_ref(a: impl AsRef<str>, b: impl AsRef<str>) -> Option<cmp::Ordering> {
    a.as_ref().partial_cmp(b.as_ref())
}

/// Generates ['cmp::PartialEq'] and ['cmp::PartialOrd'] for the `lhs` and `rhs`
/// string types, where `ty` is the unowned, reference path type.
pub macro impl_cmp_str($lhs:ty, $rhs:ty, $ty:ty) {
    impl cmp::PartialEq<$rhs> for $lhs {
        #[inline]
        fn eq(&self, other: &$rhs) -> bool {
            $crate::paths::cmp_impls::eq_as_ref(self, other)
        }
    }

    impl cmp::PartialEq<$lhs> for $rhs {
        #[inline]
        fn eq(&self, other: &$lhs) -> bool {
            $crate::paths::cmp_impls::eq_as_ref(self, other)
        }
    }

    impl cmp::PartialOrd<$rhs> for $lhs {
        #[inline]
        fn partial_cmp(&self, other: &$rhs) -> Option<cmp::Ordering> {
            $crate::paths::cmp_impls::partial_cmp_as_ref(self, other)
        }
    }

    impl cmp::PartialOrd<$lhs> for $rhs {
        #[inline]
        fn partial_cmp(&self, other: &$lhs) -> Option<cmp::Ordering> {
            $crate::paths::cmp_impls::partial_cmp_as_ref(self, other)
        }
    }
}

use crate::paths::forward_rel_path::ForwardRelativePath;
use crate::paths::forward_rel_path::ForwardRelativePathBuf;

impl_cmp!(
    ForwardRelativePathBuf,
    ForwardRelativePath,
    ForwardRelativePath
);
impl_cmp!(
    ForwardRelativePathBuf,
    &'_ ForwardRelativePath,
    ForwardRelativePath
);

impl_cmp_str!(ForwardRelativePathBuf, str, ForwardRelativePath);
impl_cmp_str!(ForwardRelativePathBuf, &'_ str, ForwardRelativePath);
impl_cmp_str!(ForwardRelativePathBuf, String, ForwardRelativePath);
impl_cmp_str!(ForwardRelativePath, str, ForwardRelativePath);
impl_cmp_str!(ForwardRelativePath, &'_ str, ForwardRelativePath);
impl_cmp_str!(ForwardRelativePath, String, ForwardRelativePath);
impl_cmp_str!(&'_ ForwardRelativePath, str, ForwardRelativePath);
impl_cmp_str!(&'_ ForwardRelativePath, String, ForwardRelativePath);
