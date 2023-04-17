/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::ffi::OsStr;
use std::fmt;
use std::ops::Deref;
use std::path::Path;
use std::path::PathBuf;

use serde::Deserialize;
use serde::Deserializer;

use crate::json_project::Edition;

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Target(String);

impl Target {
    pub fn new<T>(target: T) -> Target
    where
        T: Into<String>,
    {
        let target: String = target.into();
        let target = target.split(' ').next().expect("Unable to get raw label");
        Target(target.to_owned())
    }
}

impl<'de> Deserialize<'de> for Target {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let string: String = Deserialize::deserialize(deserializer)?;
        Ok(Target::new(string))
    }
}

impl Deref for Target {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_str()
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

/// Allows Target to be passed as an argument to Command.
impl AsRef<OsStr> for Target {
    fn as_ref(&self) -> &OsStr {
        OsStr::new(&self.0)
    }
}

impl AsRef<Path> for Target {
    fn as_ref(&self) -> &Path {
        Path::new(&self.0)
    }
}

impl AsRef<[u8]> for Target {
    fn as_ref(&self) -> &[u8] {
        self.0.as_bytes()
    }
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct MacroOutput {
    pub actual: Target,
    pub dylib: PathBuf,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct TargetInfo {
    pub name: String,
    pub label: String,
    pub edition: Option<Edition>,
    pub srcs: Vec<PathBuf>,
    /// Mapped srcs are effectively aliases. The key is a buck target
    /// of some kind, and the value is a path/filename that can be
    /// referred to in the rest of the rule.
    ///
    /// Asking buck to build the targets and tell us the output path
    /// is how we are able to support generated sources.
    pub mapped_srcs: BTreeMap<PathBuf, PathBuf>,
    #[serde(rename = "crate")]
    pub crate_name: Option<String>,
    pub crate_root: Option<PathBuf>,
    #[serde(rename = "deps", alias = "buck.direct_dependencies", default)]
    pub deps: Vec<Target>,
    // Optional set of renamed crates. in buck2, these are not unified with
    // `buck.direct_dependencies` and are instead a separate entry.
    pub named_deps: BTreeMap<String, Target>,
    pub proc_macro: Option<bool>,
    // Set of features enabled for this crate.
    pub features: Vec<String>,
    // The ensured folder containing symlinks to all sources
    pub source_folder: PathBuf,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct AliasedTargetInfo {
    pub actual: Target,
}

#[derive(Debug)]
pub struct TargetInfoEntry<'a> {
    pub index: usize,
    pub info: &'a TargetInfo,
}

impl TargetInfo {
    pub fn deps(&self) -> &'_ [Target] {
        &self.deps
    }

    pub fn crate_name(&self) -> String {
        self.crate_name.as_deref().map_or_else(
            || self.name.as_str().replace('-', "_"),
            |crate_name| crate_name.to_owned(),
        )
    }

    pub fn root_module(&self) -> PathBuf {
        // Matches buck crate_root fetching logic
        let root_candidates = if let Some(crate_root) = &self.crate_root {
            // If provided with one, use it
            vec![crate_root.to_owned()]
        } else {
            // Use buck rust build.bxl fallback logic
            vec![
                PathBuf::from("lib.rs"),
                PathBuf::from("main.rs"),
                PathBuf::from(&self.name.replace('-', "_")),
            ]
        };

        tracing::debug!(
            ?self,
            ?root_candidates,
            "trying to discover a good root module"
        );
        // for all normal sources, we need to reference the file on the fbcode tree so navigation works
        match self.srcs.iter().find(|src| {
            root_candidates
                .iter()
                .any(|candidate| src.ends_with(candidate))
        }) {
            // If a real source is provided, returns it's absolute path.
            // This will not work with crate using more than one target as a direct src.
            // Fortunately this is not used at the moment. Likely to be fixed in BXL instead
            Some(path) => return path.to_path_buf(),
            None => tracing::debug!(
                ?self,
                "unable to find root for crate; assuming it is a mapped crate"
            ),
        };

        for (dest, _) in self.mapped_srcs.iter() {
            if root_candidates.iter().any(|c| dest.ends_with(c)) {
                // Returns the files as seen in the materialized source
                return self.source_folder.join(dest);
            }
        }

        if let Some(fallback_path) = self.srcs.first().cloned().or_else(|| {
            self.mapped_srcs
                .keys()
                .next()
                .map(|mapped_path| self.source_folder.join(mapped_path))
        }) {
            return fallback_path;
        }

        tracing::error!(?self, "no crate root can be found");

        panic!("Invariant broken: rust-project is unable to determine a root module")
    }
}
