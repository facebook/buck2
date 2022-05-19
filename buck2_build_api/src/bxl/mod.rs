/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! bxl is the Buck Extension Language, allowing any integrator to write Starlark code that
//! introspects buck2 internal graphs in a safe, incremental way to perform more complex operations
//!

use std::sync::Arc;

use buck2_interpreter::common::BxlFilePath;
use derive_more::Display;
use gazebo::prelude::*;
use serde::{Serialize, Serializer};
use starlark::collections::SmallMap;
// TODO(brasselsprouts): This re-export is only here so that `bql` can access StarlarkTargetSet. Remove once bql is deprecated.
pub use starlark_defs::targetset::StarlarkTargetSet;

use crate::bxl::starlark_defs::cli_args::CliArgValue;

pub mod calculation;
pub mod common;
pub mod eval;
pub mod result;
pub mod starlark_defs;

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

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Ord, PartialOrd)]
#[display(fmt = "{}", .0)]
pub struct BxlKey(pub(crate) Arc<BxlKeyData>);

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
#[display(fmt = "BxlFunction({},{:?})", "spec", "bxl_args")]
pub(crate) struct BxlKeyData {
    pub(crate) spec: BxlFunctionLabel,
    pub(crate) bxl_args: Arc<SmallMap<String, CliArgValue>>,
}
