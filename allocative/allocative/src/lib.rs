/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! # Allocative
//!
//! Crate implements lightweight memory profiler which allows
//! object traversal and size introspection.
//!
//! An object implementing [`Allocative`] trait is introspectable, and this crate
//! provides two utilities to work with such objects:
//! * [`FlameGraphBuilder`] to build a flame graph of object tree
//! * [`size_of_unique_allocated_data`] provides estimation
//!    of how much allocated memory the value holds
//!
//! ## Allocative overhead
//!
//! When allocative is used, binary size is slightly increased due to implementations
//! of [`Allocative`] trait, but it has no runtime/memory overhead when it is not used.
//!
//! ## How it is different from other call-stack malloc profilers like jemalloc heap profiler
//!
//! Allocative is not a substitute for call stack malloc profiler,
//! it provides a different view on memory usage.
//!
//! Here are some differences between allocative and call-stack malloc profiler:
//!
//! * Allocative requires implementation of [`Allocative`] trait for each type
//!   which needs to be measured, and some setup in the program to enable it
//! * Allocative flamegraph shows object by object tree, not by call stack
//! * Allocative shows gaps in allocated memory,
//!   e.g. spare capacity of collections or too large padding in structs or enums
//! * Allocative allows profiling non-malloc allocations (for example, allocations within [bumpalo])
//! * Allocative allows profiling of memory for subset of the process data
//!   (for example, measure the size of RPC response before serialization)
//!
//! [bumpalo]: https://github.com/fitzgen/bumpalo

#![cfg_attr(rust_nightly, feature(const_type_name))]
#![cfg_attr(rust_nightly, feature(never_type))]
#![deny(rustdoc::broken_intra_doc_links)]

mod allocative_trait;
mod flamegraph;
mod global_root;
mod impls;
mod key;
mod rc_str;
mod size_of;
mod test_derive;
mod visitor;

pub use allocative_derive::root;
pub use allocative_derive::Allocative;

pub use crate::allocative_trait::Allocative;
pub use crate::flamegraph::FlameGraphBuilder;
pub use crate::global_root::register_root;
pub use crate::key::Key;
pub use crate::size_of::size_of_unique_allocated_data;
pub use crate::visitor::Visitor;

#[doc(hidden)]
pub mod __macro_refs {
    pub use ctor;
}
