/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A build-system independent project structure defined and used by rust-analyzer.
//!
//! While this format is provisional and subject to change, `rust-project` and Rusty
//! take a necessary dependency on these definitions to provide IDE functionality to
//! buck-based projects. For additional details, see rust-analyzer's [documentation].
//!
//! [documentation]: https://rust-analyzer.github.io/manual.html#non-cargo-based-projects

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::path::PathBuf;

use serde::Deserialize;
use serde::Serialize;

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct JsonProject {
    /// Path to the directory of the sysroot; this is a superset of `sysroot_src`.
    ///
    /// This path provides rust-analyzer both the *source code* of libraries
    /// like `std` and `core` and binaries like `rust-analyzer-proc-macro-srv`,
    /// which enable rust-analyzer to expand procedural macros.
    ///
    /// For example, a `sysroot` is `~/fbsource/fbcode/third-party-buck/platform010/build/rust/`.
    ///
    /// `rust-analyzer` now relies on an external binary to expand procedural
    /// macros and the source code location can be predictably inferred.
    /// Assuming the example sysroot above, the source code would be located in
    /// `/lib/rustlib/src/rust/`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sysroot: Option<PathBuf>,

    /// Legacy sysroot config containing only the `std` and `core` source code.
    /// Useful on non-linux platforms, but procedural macro expansion won't
    /// work. Can be specified in addition to `sysroot` to override source
    /// location.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sysroot_src: Option<PathBuf>,

    /// The set of crates comprising the project.
    ///
    /// Must include all transitive dependencies as well as sysroot crate (libstd,
    /// libcore, etc.).
    pub crates: Vec<Crate>,
    pub generated: &'static str,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct Crate {
    /// Optional crate name used for display purposes; has no semantic significance.
    pub display_name: Option<String>,
    /// The path to the root module of the crate.
    pub root_module: PathBuf,
    pub edition: Edition,
    pub deps: Vec<Dep>,
    /// Should this crate be treated as a member of
    /// current "workspace".
    ///
    /// By default, inferred from the `root_module`
    /// (members are the crates which reside inside
    /// the directory opened in the editor).
    ///
    /// Set this to `false` for things like standard
    /// library and 3rd party crates to enable
    /// performance optimizations (rust-analyzer
    /// assumes that non-member crates don't change).
    pub is_workspace_member: bool,
    /// Optionally specify the (super)set of `.rs`
    /// files comprising this crate.
    ///
    /// By default, rust-analyzer assumes that only
    /// files under `root_module.parent` can belong
    /// to a crate. `include_dirs` are included
    /// recursively, unless a subdirectory is in
    /// `exclude_dirs`.
    ///
    /// Different crates can share the same `source`.
    ///
    /// If two crates share an `.rs` file in common,
    /// they *must* have the same `source`.
    /// rust-analyzer assumes that files from one
    /// source can't refer to files in another source.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<Source>,
    /// The set of cfgs activated for a given crate.
    ///
    /// With how fb imports crates into fbsource/third-party,
    /// the answer is "all of them".
    pub cfg: Vec<String>,
    /// The target triple for a given crate.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target: Option<String>,
    /// Environment for the crate, often used by `env!`.
    pub env: BTreeMap<String, String>,
    /// Whether the crate is a proc-macro crate/
    pub is_proc_macro: bool,
    /// For proc-macro crates, path to compiled
    /// proc-macro (.so, .dylib, or .dll. depends on the platform.)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub proc_macro_dylib_path: Option<PathBuf>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(rename = "edition")]
pub enum Edition {
    #[serde(rename = "2015")]
    Edition2015,
    #[serde(rename = "2018")]
    Edition2018,
    #[serde(rename = "2021")]
    Edition2021,
}

/// An optional set of Rust files that comprise the crate.
///
/// By default, rust-analyzer assumes that only files under
/// `Crate::root_module` can belong to a crate. `include_dirs`
/// are included recursively, unless a subdirectory is
/// specified in `include_dirs`.
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Default)]
pub struct Source {
    pub include_dirs: BTreeSet<PathBuf>,
    pub exclude_dirs: BTreeSet<PathBuf>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub struct Dep {
    #[serde(rename = "crate")]
    pub crate_index: usize,
    pub name: String,
}
