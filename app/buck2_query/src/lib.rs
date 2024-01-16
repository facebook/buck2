/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]
#![feature(async_closure)]
#![feature(box_patterns)]
#![feature(return_position_impl_trait_in_trait)]
#![feature(try_blocks)]

pub mod query;

pub use buck2_query_derive::query_module;

// Required for use of #[query_module] within this crate (it allows query_module generated code to reference this crate as
// ::buck2_query like it would when used in other crates).
extern crate self as buck2_query;

/// __derive_refs allows us to reference other crates in buck_query_proc_macro without users needing to be
///  aware of those dependencies. We make them public here and then can reference them like
///  `buck_query::__derive_refs::foo`.
#[doc(hidden)]
pub mod __derive_refs {
    pub use async_trait;
    pub use buck2_query_parser;
    pub use indexmap;
    pub use ref_cast;
}
