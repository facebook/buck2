/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! General macros useful for path declaration
//!

use std::cmp;

///
/// Generates ['cmp::PartialEq'] and ['cmp::PartialOrd'] for the `lhs` and `rhs`
/// types, where `ty` is the unowned, reference path type.
macro_rules! impl_cmp {
    ($lhs:ty, $rhs:ty, $ty:ty) => {
        impl<'a, 'b> cmp::PartialEq<$rhs> for $lhs {
            #[inline]
            fn eq(&self, other: &$rhs) -> bool {
                <$ty as cmp::PartialEq>::eq(self, other)
            }
        }

        impl<'a, 'b> cmp::PartialEq<$lhs> for $rhs {
            #[inline]
            fn eq(&self, other: &$lhs) -> bool {
                <$ty as cmp::PartialEq>::eq(self, other)
            }
        }

        impl<'a, 'b> cmp::PartialOrd<$rhs> for $lhs {
            #[inline]
            fn partial_cmp(&self, other: &$rhs) -> Option<cmp::Ordering> {
                <$ty as cmp::PartialOrd>::partial_cmp(self, other)
            }
        }

        impl<'a, 'b> cmp::PartialOrd<$lhs> for $rhs {
            #[inline]
            fn partial_cmp(&self, other: &$lhs) -> Option<cmp::Ordering> {
                <$ty as cmp::PartialOrd>::partial_cmp(self, other)
            }
        }
    };
}

///
/// Generates ['cmp::PartialEq'] and ['cmp::PartialOrd'] for the `lhs` and `rhs`
/// string types, where `ty` is the unowned, reference path type.
macro_rules! impl_cmp_str {
    ($lhs:ty, $rhs:ty, $ty:ty) => {
        impl<'a, 'b> cmp::PartialEq<$rhs> for $lhs {
            #[inline]
            fn eq(&self, other: &$rhs) -> bool {
                match <$ty>::new(other) {
                    Ok(other) => <$ty as cmp::PartialEq>::eq(self, other),
                    _ => false,
                }
            }
        }

        impl<'a, 'b> cmp::PartialEq<$lhs> for $rhs {
            #[inline]
            fn eq(&self, other: &$lhs) -> bool {
                match <$ty>::new(self) {
                    Ok(this) => <$ty as cmp::PartialEq>::eq(this, other),
                    _ => false,
                }
            }
        }

        impl<'a, 'b> cmp::PartialOrd<$rhs> for $lhs {
            #[inline]
            fn partial_cmp(&self, other: &$rhs) -> Option<cmp::Ordering> {
                match <$ty>::new(other) {
                    Ok(other) => <$ty as cmp::PartialOrd>::partial_cmp(self, other),
                    _ => None,
                }
            }
        }

        impl<'a, 'b> cmp::PartialOrd<$lhs> for $rhs {
            #[inline]
            fn partial_cmp(&self, other: &$lhs) -> Option<cmp::Ordering> {
                match <$ty>::new(self) {
                    Ok(this) => <$ty as cmp::PartialOrd>::partial_cmp(this, other),
                    _ => None,
                }
            }
        }
    };
}

use crate::fs::paths::{ForwardRelativePath, ForwardRelativePathBuf};

impl_cmp!(
    ForwardRelativePathBuf,
    ForwardRelativePath,
    ForwardRelativePath
);
impl_cmp!(
    ForwardRelativePathBuf,
    &'a ForwardRelativePath,
    ForwardRelativePath
);

impl_cmp_str!(ForwardRelativePathBuf, str, ForwardRelativePath);
impl_cmp_str!(ForwardRelativePathBuf, &'a str, ForwardRelativePath);
impl_cmp_str!(ForwardRelativePathBuf, String, ForwardRelativePath);
impl_cmp_str!(ForwardRelativePath, str, ForwardRelativePath);
impl_cmp_str!(ForwardRelativePath, &'a str, ForwardRelativePath);
impl_cmp_str!(ForwardRelativePath, String, ForwardRelativePath);
impl_cmp_str!(&'a ForwardRelativePath, str, ForwardRelativePath);
impl_cmp_str!(&'a ForwardRelativePath, String, ForwardRelativePath);

use crate::fs::paths::{AbsPath, AbsPathBuf};

impl_cmp!(AbsPathBuf, AbsPath, AbsPath);
impl_cmp!(AbsPathBuf, &'a AbsPath, AbsPath);

impl_cmp_str!(AbsPathBuf, str, AbsPath);
impl_cmp_str!(AbsPathBuf, &'a str, AbsPath);
impl_cmp_str!(AbsPathBuf, String, AbsPath);
impl_cmp_str!(AbsPath, str, AbsPath);
impl_cmp_str!(AbsPath, &'a str, AbsPath);
impl_cmp_str!(AbsPath, String, AbsPath);
impl_cmp_str!(&'a AbsPath, str, AbsPath);
impl_cmp_str!(&'a AbsPath, String, AbsPath);

use crate::package::{PackageRelativePath, PackageRelativePathBuf};

impl_cmp!(
    PackageRelativePathBuf,
    PackageRelativePath,
    PackageRelativePath
);
impl_cmp!(
    PackageRelativePathBuf,
    &'a PackageRelativePath,
    PackageRelativePath
);

impl_cmp_str!(PackageRelativePathBuf, str, PackageRelativePath);
impl_cmp_str!(PackageRelativePathBuf, &'a str, PackageRelativePath);
impl_cmp_str!(PackageRelativePathBuf, String, PackageRelativePath);
impl_cmp_str!(PackageRelativePath, str, PackageRelativePath);
impl_cmp_str!(PackageRelativePath, &'a str, PackageRelativePath);
impl_cmp_str!(PackageRelativePath, String, PackageRelativePath);
impl_cmp_str!(&'a PackageRelativePath, str, PackageRelativePath);
impl_cmp_str!(&'a PackageRelativePath, String, PackageRelativePath);

use crate::fs::project::{ProjectRelativePath, ProjectRelativePathBuf};

impl_cmp!(
    ProjectRelativePathBuf,
    ProjectRelativePath,
    ProjectRelativePath
);
impl_cmp!(
    ProjectRelativePathBuf,
    &'a ProjectRelativePath,
    ProjectRelativePath
);

impl_cmp_str!(ProjectRelativePathBuf, str, ProjectRelativePath);
impl_cmp_str!(ProjectRelativePathBuf, &'a str, ProjectRelativePath);
impl_cmp_str!(ProjectRelativePathBuf, String, ProjectRelativePath);
impl_cmp_str!(ProjectRelativePath, str, ProjectRelativePath);
impl_cmp_str!(ProjectRelativePath, &'a str, ProjectRelativePath);
impl_cmp_str!(ProjectRelativePath, String, ProjectRelativePath);
impl_cmp_str!(&'a ProjectRelativePath, str, ProjectRelativePath);
impl_cmp_str!(&'a ProjectRelativePath, String, ProjectRelativePath);
