/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::ffi::OsStr;
use std::fmt;
use std::fs;
use std::ops::Deref;
use std::path::Path;
use std::path::PathBuf;

use rustc_hash::FxHashMap;
use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::de::Error as _;
use serde::de::MapAccess;
use serde::de::SeqAccess;
use serde::de::Visitor;

use crate::path::canonicalize_to_vcs_path;
use crate::project_json::Edition;

#[derive(Serialize, Debug, Default, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub(crate) struct Target(String);

impl Target {
    pub(crate) fn new<T>(target: T) -> Target
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
pub(crate) struct MacroOutput {
    pub(crate) actual: Target,
    pub(crate) dylib: PathBuf,
}
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub(crate) enum Kind {
    #[serde(rename = "prelude//rules.bzl:rust_binary")]
    Binary,
    #[serde(rename = "prelude//rules.bzl:rust_library")]
    Library,
    #[serde(rename = "prelude//rules.bzl:rust_test")]
    Test,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub(crate) struct TargetInfo {
    pub(crate) name: String,
    pub(crate) label: String,
    pub(crate) kind: Kind,
    pub(crate) edition: Option<Edition>,
    pub(crate) srcs: Vec<PathBuf>,
    /// Mapped srcs are effectively aliases. The key is a buck target
    /// of some kind, and the value is a path/filename that can be
    /// referred to in the rest of the rule.
    ///
    /// Asking buck to build the targets and tell us the output path
    /// is how we are able to support generated sources.
    pub(crate) mapped_srcs: FxHashMap<PathBuf, PathBuf>,
    #[serde(rename = "crate")]
    pub(crate) crate_name: Option<String>,
    pub(crate) crate_dynamic: Option<PathBuf>,
    pub(crate) crate_root: PathBuf,
    pub(crate) deps: Vec<Target>,
    #[serde(rename = "tests")]
    pub(crate) test_deps: Vec<Target>,
    // Optional set of renamed crates. in buck2, these are not unified with
    // `buck.direct_dependencies` and are instead a separate entry.
    #[serde(deserialize_with = "deserialize_named_deps")]
    pub(crate) named_deps: FxHashMap<String, Target>,
    pub(crate) proc_macro: Option<bool>,
    // Set of features enabled for this crate.
    pub(crate) features: Vec<String>,
    pub(crate) env: FxHashMap<String, String>,
    // The ensured folder containing symlinks to all sources
    pub(crate) source_folder: PathBuf,
    pub(crate) project_relative_buildfile: PathBuf,
    pub(crate) in_workspace: bool,
    pub(crate) rustc_flags: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub(crate) struct AliasedTargetInfo {
    pub(crate) actual: Target,
}

impl TargetInfo {
    pub(crate) fn crate_name(&self) -> String {
        if let Some(crate_dynamic) = &self.crate_dynamic {
            if let Ok(contents) = fs::read_to_string(crate_dynamic) {
                return contents.trim().to_owned();
            }
        }
        self.crate_name.as_deref().map_or_else(
            || self.name.as_str().replace('-', "_"),
            |crate_name| crate_name.to_owned(),
        )
    }

    pub(crate) fn display_name(&self) -> String {
        if self.name.chars().all(|c| c.is_ascii_digit() || c == '.') {
            // For target of the form foo:1.2.3, the buck name is 1.2.3 but
            // that's not useful for a display name.
            self.crate_name()
        } else {
            self.name
                .strip_suffix("-unittest")
                .unwrap_or(&self.name)
                .to_owned()
        }
    }

    pub(crate) fn root_module(&self, project_root: &Path) -> PathBuf {
        let p = self.source_folder.join(&self.crate_root);
        canonicalize_to_vcs_path(&p, project_root)
    }

    pub(crate) fn overridden_dep_names(&self) -> FxHashMap<Target, String> {
        let mut overridden = FxHashMap::default();
        for (name, target) in &self.named_deps {
            overridden.insert(target.clone(), name.to_owned());
        }

        overridden
    }

    pub(crate) fn cfg(&self) -> Vec<String> {
        // we need to take the existing features and prefix `feature=`
        let feature_cfgs = self.features.iter().map(|f| format!("feature=\"{f}\""));

        // parse out rustc --cfg= flags
        let rustc_flags_cfgs = self.rustc_flags.iter().flat_map(|flag| {
            if let Some(atfile) = flag.strip_prefix("@") {
                // If we fail to expand, continue anyways to provide a nearly working experience.
                expand_atfile(Path::new(atfile)).unwrap_or_default()
            } else if let Some(cfg) = flag.strip_prefix("--cfg=") {
                vec![cfg.to_owned()]
            } else {
                vec![]
            }
        });

        feature_cfgs
            .chain(rustc_flags_cfgs)
            .collect::<Vec<String>>()
    }
}

#[derive(Debug, Clone, Deserialize, PartialEq, Eq, Default)]
pub(crate) struct ExpandedAndResolved {
    pub(crate) expanded_targets: Vec<Target>,
    pub(crate) queried_proc_macros: FxHashMap<Target, MacroOutput>,
    pub(crate) resolved_deps: FxHashMap<Target, TargetInfo>,
}

fn deserialize_named_deps<'de, D>(deserializer: D) -> Result<FxHashMap<String, Target>, D::Error>
where
    D: Deserializer<'de>,
{
    struct NamedDepsVisitor;

    impl<'de> Visitor<'de> for NamedDepsVisitor {
        type Value = FxHashMap<String, Target>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("dict or list")
        }

        fn visit_map<M>(self, map: M) -> Result<Self::Value, M::Error>
        where
            M: MapAccess<'de>,
        {
            FxHashMap::deserialize(serde::de::value::MapAccessDeserializer::new(map))
        }

        fn visit_seq<S>(self, mut seq: S) -> Result<Self::Value, S::Error>
        where
            S: SeqAccess<'de>,
        {
            let names: PathBuf = seq
                .next_element()?
                .ok_or_else(|| S::Error::invalid_length(0, &self))?;
            let content = fs::read_to_string(&names).map_err(|e| {
                S::Error::custom(format!("failed to read {}: {}", names.display(), e))
            })?;
            let mut lines = content.lines();

            let mut named_deps = FxHashMap::default();
            while let Some(target) = seq.next_element()? {
                let name = lines.next().ok_or_else(|| {
                    S::Error::custom(format!("not enough lines in {}", names.display()))
                })?;
                named_deps.insert(name.to_owned(), target);
            }
            Ok(named_deps)
        }
    }

    deserializer.deserialize_any(NamedDepsVisitor)
}

fn expand_atfile(path: &Path) -> Result<Vec<String>, anyhow::Error> {
    let contents = fs::read_to_string(path)?;
    let flags = contents
        .lines()
        .filter_map(|flag| flag.strip_prefix("--cfg=").map(str::to_string));
    Ok(flags.collect::<Vec<String>>())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cfg() {
        let info = TargetInfo {
            name: "bar".to_owned(),
            label: "bar".to_owned(),
            kind: Kind::Library,
            edition: None,
            srcs: vec![],
            mapped_srcs: FxHashMap::default(),
            crate_name: None,
            crate_dynamic: None,
            crate_root: PathBuf::default(),
            deps: vec![],
            test_deps: vec![],
            named_deps: FxHashMap::default(),
            proc_macro: None,
            features: vec!["foo_feature".to_owned()],
            env: FxHashMap::default(),
            source_folder: PathBuf::from("/tmp"),
            project_relative_buildfile: PathBuf::from("bar/BUCK"),
            in_workspace: false,
            rustc_flags: vec!["--cfg=foo_cfg".to_owned(), "--other".to_owned()],
        };

        assert_eq!(
            info.cfg(),
            vec!["feature=\"foo_feature\"".to_owned(), "foo_cfg".to_owned()]
        );
    }

    #[test]
    fn test_display_name_version() {
        let info = TargetInfo {
            name: "1.2.3".to_owned(),
            label: "//third-party/foo:1.2.3".to_owned(),
            kind: Kind::Library,
            edition: None,
            srcs: vec![],
            mapped_srcs: FxHashMap::default(),
            crate_name: Some("foo".to_owned()),
            crate_dynamic: None,
            crate_root: PathBuf::default(),
            deps: vec![],
            test_deps: vec![],
            named_deps: FxHashMap::default(),
            proc_macro: None,
            features: vec![],
            env: FxHashMap::default(),
            source_folder: PathBuf::from("/tmp"),
            project_relative_buildfile: PathBuf::from("third-party/BUCK"),
            in_workspace: false,
            rustc_flags: vec![],
        };
        assert_eq!(info.display_name(), "foo");
    }

    #[test]
    fn test_display_name_strips_unittest_suffix() {
        let info = TargetInfo {
            name: "my_crate-unittest".to_owned(),
            label: "//foo:my_crate-unittest".to_owned(),
            kind: Kind::Test,
            edition: None,
            srcs: vec![],
            mapped_srcs: FxHashMap::default(),
            crate_name: None,
            crate_dynamic: None,
            crate_root: PathBuf::default(),
            deps: vec![],
            test_deps: vec![],
            named_deps: FxHashMap::default(),
            proc_macro: None,
            features: vec![],
            env: FxHashMap::default(),
            source_folder: PathBuf::from("/tmp"),
            project_relative_buildfile: PathBuf::from("foo/BUCK"),
            in_workspace: false,
            rustc_flags: vec![],
        };

        assert_eq!(info.display_name(), "my_crate");
    }
}
