/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![deny(warnings, missing_docs, clippy::all, broken_intra_doc_links)]
#![allow(clippy::needless_doctest_main, clippy::new_without_default)]
//! Provides [FacebookInit] structure that must be used in Facebook code that
//! requires pre-initialization, e.g. like C++'s logging.

#[cfg(not(fbcode_build))]
mod oss;

pub use macros::{compat_test, main, test};

#[cfg(fbcode_build)]
use quickcheck as _; // used in oss

#[cfg(fbcode_build)]
pub use fbinit::*;

#[cfg(not(fbcode_build))]
pub use oss::*;
