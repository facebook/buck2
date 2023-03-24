/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::base_deferred_key_dyn::BaseDeferredKeyDynImpl;
use buck2_core::collections::ordered_map::OrderedMap;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::TargetLabel;
use buck2_data::ToProtoMessage;
use buck2_interpreter::path::BxlFilePath;
use derive_more::Display;
use dupe::Dupe;
use gazebo::cmp::PartialEqAny;
use itertools::Itertools;
use serde::Serialize;
use serde::Serializer;

#[derive(
    Debug, Display, PartialEq, Eq, Clone, Hash, Ord, PartialOrd, Allocative
)]
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
    #[display(fmt = "{}", "_0.iter().map(|v| v.to_string()).join(\",\")")]
    List(Vec<CliArgValue>),
    None,
    TargetLabel(TargetLabel),
    ProvidersLabel(ProvidersLabel),
}

#[derive(
    Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Ord, PartialOrd, Allocative
)]
pub struct BxlKey(Arc<BxlKeyData>);

impl BxlKey {
    pub fn new(
        spec: BxlFunctionLabel,
        bxl_args: Arc<OrderedMap<String, CliArgValue>>,
        global_target_platform: Option<TargetLabel>,
    ) -> Self {
        Self(Arc::new(BxlKeyData {
            spec,
            bxl_args,
            global_target_platform,
        }))
    }

    pub fn label(&self) -> &BxlFunctionLabel {
        &self.0.spec
    }

    pub fn cli_args(&self) -> &Arc<OrderedMap<String, CliArgValue>> {
        &self.0.bxl_args
    }

    pub fn into_base_deferred_key_dyn_impl(self) -> Arc<dyn BaseDeferredKeyDynImpl> {
        self.0
    }

    pub fn global_target_platform(&self) -> &Option<TargetLabel> {
        &self.0.global_target_platform
    }
}

#[derive(
    Clone, Display, Debug, Eq, Hash, PartialEq, Ord, PartialOrd, Allocative
)]
#[display(fmt = "{} ({})", "spec", "print_like_args(bxl_args)")]
struct BxlKeyData {
    spec: BxlFunctionLabel,
    bxl_args: Arc<OrderedMap<String, CliArgValue>>,
    global_target_platform: Option<TargetLabel>,
}

fn print_like_args(args: &Arc<OrderedMap<String, CliArgValue>>) -> String {
    args.iter()
        .map(|(arg, argv)| format!("--{}={}", arg, argv))
        .join(" ")
}

impl BaseDeferredKeyDynImpl for BxlKeyData {
    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }

    fn hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        Hash::hash(self, &mut hasher);
        hasher.finish()
    }

    fn make_hashed_path(
        &self,
        base: &ProjectRelativePath,
        prefix: &ForwardRelativePath,
        action_key: Option<&str>,
        path: &ForwardRelativePath,
    ) -> ProjectRelativePathBuf {
        let label = &self.spec;
        let cell_relative_path = label.bxl_path.path().path().as_str();

        let output_hash = {
            let mut hasher = DefaultHasher::new();
            self.bxl_args.hash(&mut hasher);
            let output_hash = hasher.finish();
            format!("{:x}", output_hash)
        };

        // It is performance critical that we use slices and allocate via `join` instead of
        // repeated calls to `join` on the path object because `join` allocates on each call,
        // which has a significant impact.
        let parts = [
            base.as_str(),
            "/",
            prefix.as_str(),
            "-bxl/",
            label.bxl_path.cell().as_str(),
            "/",
            output_hash.as_str(),
            "/",
            cell_relative_path,
            if cell_relative_path.is_empty() {
                ""
            } else {
                "/"
            },
            "__",
            label.name.as_str(),
            "__",
            action_key.unwrap_or_default(),
            if action_key.is_none() { "" } else { "__" },
            "/",
            path.as_str(),
        ];

        ProjectRelativePathBuf::unchecked_new(parts.concat())
    }
}

impl ToProtoMessage for BxlKey {
    type Message = buck2_data::BxlFunctionKey;

    fn as_proto(&self) -> Self::Message {
        buck2_data::BxlFunctionKey {
            label: Some(self.label().as_proto()),
        }
    }
}

/// The identifier used to find the implementation function for this bxl. Should point at the output of `bxl()`
#[derive(
    Debug, Clone, Display, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative
)]
#[display(fmt = "{}:{}", bxl_path, name)]
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

impl ToProtoMessage for BxlFunctionLabel {
    type Message = buck2_data::BxlFunctionLabel;

    fn as_proto(&self) -> Self::Message {
        buck2_data::BxlFunctionLabel {
            bxl_path: self.bxl_path.to_string(),
            name: self.name.clone(),
        }
    }
}
