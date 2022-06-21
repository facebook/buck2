/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

use std::sync::Arc;

use buck2_core::provider::ProvidersLabel;
use buck2_core::target::TargetLabel;
use buck2_interpreter::common::BxlFilePath;
use derive_more::Display;
use gazebo::dupe::Dupe;
use itertools::Itertools;
use serde::Serialize;
use serde::Serializer;
use starlark::collections::SmallMap;

#[derive(Debug, Display, PartialEq, Eq, Clone, Hash, Ord, PartialOrd)]
pub enum CliArgValue {
    Bool(bool),
    Int(i32),
    // store this as a string here for eq, hash since generally this comes from cmdline.
    // Note that this means `3.0` and `3.00` would not be equal. The string should already have
    // been verified to be a f64.
    Float(String),
    String(String),
    // Type of list elements is used to verify that concatenation is valid.
    // That only can be checked after configuration took place,
    // so pass the type info together with values to be used later.
    #[display(fmt = "_0.iter().map(|v| v.to_string()).join(',')")]
    List(Vec<CliArgValue>),
    None,
    TargetLabel(TargetLabel),
    ProvidersLabel(ProvidersLabel),
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Ord, PartialOrd)]
#[display(fmt = "{}", .0)]
pub struct BxlKey(Arc<BxlKeyData>);

impl BxlKey {
    pub fn new(spec: BxlFunctionLabel, bxl_args: Arc<SmallMap<String, CliArgValue>>) -> Self {
        Self(Arc::new(BxlKeyData { spec, bxl_args }))
    }

    pub fn label(&self) -> &BxlFunctionLabel {
        &self.0.spec
    }

    pub fn cli_args(&self) -> &Arc<SmallMap<String, CliArgValue>> {
        &self.0.bxl_args
    }
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Ord, PartialOrd)]
#[display(fmt = "{} ({})", "spec", "print_like_args(bxl_args)")]
pub(crate) struct BxlKeyData {
    pub(crate) spec: BxlFunctionLabel,
    pub(crate) bxl_args: Arc<SmallMap<String, CliArgValue>>,
}

fn print_like_args(args: &Arc<SmallMap<String, CliArgValue>>) -> String {
    args.iter()
        .map(|(arg, argv)| format!("--{}={}", arg, argv))
        .join(" ")
}

/// The identifier used to find the implementation function for this bxl. Should point at the output of `bxl()`
#[derive(Debug, Clone, Display, Eq, PartialEq, Hash, Ord, PartialOrd)]
#[display(fmt = "{}:{}", "bxl_path.id()", name)]
pub struct BxlFunctionLabel {
    /// The cell, package, and file that contains the output of `bxl()`
    pub bxl_path: BxlFilePath,
    /// The name of the symbol that is bound to the output of `bxl()`, e.g. `custom_query`
    pub name: String,
}

impl Serialize for BxlFunctionLabel {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.serialize_str(&format!("{}", self))
    }
}
