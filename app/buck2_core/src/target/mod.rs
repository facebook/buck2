/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! A 'target' is an instance of a rule declared in the build file. Each
//! 'target' is a node on the 'static graph'. Targets are determined by parsing;
//! no extra analysis or building is necessary to determine the 'target's
//! available.
//!
//! For example, the below is a target, defined in some 'Package' with the given
//! name "foo".
//!
//!```ignored
//! java_library(
//!    name = "foo",
//!    srcs = [ ... ],
//!    ...
//! )
//! ```
//!
//! Target names are limited to non-empty alpha numeric characters `,`, `=`,
//! `-`, `/`, and `_`. No other special characters, e.g. spaces, are allowed.
//! Currently, `+` is allow for backwards compatibility but may be removed.
//!
//! 'TargetLabel's are labels/keys that uniquely map to a 'target' in the static
//! graph. These are of the form "<cell>//<path to build file>:<target name>".
//! e.g. `fbsource//my/package/path:my_target`, where `fbsource` is the cell,
//! `my/package/path` is the package, and `my_target` is the target name
//! belonging to the package.

use std::borrow::Borrow;
use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use allocative::Allocative;
use derive_more::Display;
use gazebo::prelude::*;
use serde::ser::Serialize;
use serde::ser::Serializer;
use thiserror::Error;

use crate::ascii_char_set::AsciiCharSet;
use crate::configuration::Configuration;
use crate::package::Package;

/// 'TargetName' is the name given to a particular target.
/// e.g. `foo` in the label `fbsource//package/path:foo`.
#[derive(
    Clone, Debug, Dupe, Display, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative
)]
// TODO intern this?
pub struct TargetName(Arc<str>);

#[derive(Error, Debug)]
enum InvalidTarget {
    #[error(
        "Invalid target name `{}`. Target names are non-empty strings and can only contain alpha numeric characters, and symbols \
        `,`, `.`, `=`, `-`, `/`, `~`, `@`, `!`, `+`, `$`, and `_`. No other characters are allowed.",
        _0
    )]
    InvalidName(String),
    #[error(
        "found inner providers label when target names are expected. remove `[...]` portion of the target name from `{}`",
        _0
    )]
    FoundProvidersLabel(String),
}

impl TargetName {
    pub fn new(name: &str) -> anyhow::Result<Self> {
        if Self::verify(name) {
            Ok(Self(Arc::from(name)))
        } else {
            if let Some((_, p)) = name.split_once('[') {
                if p.contains(']') {
                    return Err(anyhow::anyhow!(InvalidTarget::FoundProvidersLabel(
                        name.to_owned()
                    )));
                }
            }
            Err(anyhow::anyhow!(InvalidTarget::InvalidName(name.to_owned())))
        }
    }

    pub fn unchecked_new(name: &str) -> Self {
        Self(Arc::from(name))
    }

    fn verify(name: &str) -> bool {
        const VALID_CHARS: &str =
            r"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_,.=-\/~@!+$";
        const SET: AsciiCharSet = AsciiCharSet::new(VALID_CHARS);

        !name.is_empty() && name.as_bytes().iter().all(|&b| SET.contains(b))
    }

    pub fn value(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for TargetName {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Borrow<str> for TargetName {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl PartialEq<String> for TargetName {
    fn eq(&self, other: &String) -> bool {
        *self.0 == *other
    }
}

impl PartialEq<str> for TargetName {
    fn eq(&self, other: &str) -> bool {
        *self.0 == *other
    }
}

/// 'TargetLabel' that uniquely maps to a 'target'
/// It contains a 'Package' which is the 'Package' defined by the build fine
/// that contains this 'target', and a 'name' which is a 'TargetName'
/// representing the target name given to the particular target.
#[derive(
    Clone, Dupe, Debug, Display, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative
)]
#[display(fmt = "{}:{}", pkg, name)]
pub struct TargetLabel {
    pkg: Package,
    name: TargetName,
}

impl TargetLabel {
    pub fn new(pkg: Package, name: TargetName) -> Self {
        TargetLabel { pkg, name }
    }

    pub fn pkg(&self) -> &Package {
        &self.pkg
    }

    pub fn name(&self) -> &TargetName {
        &self.name
    }

    /// Creates a 'ConfiguredTargetLabel' from ['Self'] based on the provided
    /// configuration.
    pub fn configure(&self, cfg: Configuration) -> ConfiguredTargetLabel {
        ConfiguredTargetLabel {
            target: self.dupe(),
            cfg,
            exec_cfg: None,
        }
    }

    /// Like `configure`, but forces the execution configuration too.
    pub fn configure_with_exec(
        &self,
        cfg: Configuration,
        exec_cfg: Configuration,
    ) -> ConfiguredTargetLabel {
        ConfiguredTargetLabel {
            target: self.dupe(),
            cfg,
            exec_cfg: Some(exec_cfg),
        }
    }
}

impl Serialize for TargetLabel {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

/// 'ConfiguredTargetLabel' are 'TargetLabel's with an 'Configuration' attached.
/// These uniquely map to nodes of the build graph with 'Configuration's
/// applied.
#[derive(Clone, Dupe, Debug, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative)]
pub struct ConfiguredTargetLabel {
    target: TargetLabel,
    cfg: Configuration,
    /// Usually this is None, but for toolchain deps where the exec_cfg isn't picked it is set
    exec_cfg: Option<Configuration>,
}

impl Display for ConfiguredTargetLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.target, self.cfg)?;
        if let Some(exec_cfg) = &self.exec_cfg {
            write!(f, " ({})", exec_cfg)?;
        }
        Ok(())
    }
}

impl ConfiguredTargetLabel {
    pub fn pkg(&self) -> &Package {
        &self.target.pkg
    }

    pub fn name(&self) -> &TargetName {
        &self.target.name
    }

    pub fn unconfigured(&self) -> &TargetLabel {
        &self.target
    }

    pub fn cfg(&self) -> &Configuration {
        &self.cfg
    }

    pub fn exec_cfg(&self) -> Option<&Configuration> {
        self.exec_cfg.as_ref()
    }
}

impl Serialize for ConfiguredTargetLabel {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(self)
    }
}

/// [`TargetLabel`] or [`ConfiguredTargetLabel`]
pub trait TargetLabelMaybeConfigured {}

impl TargetLabelMaybeConfigured for TargetLabel {}
impl TargetLabelMaybeConfigured for ConfiguredTargetLabel {}

pub mod testing {
    use crate::configuration::Configuration;
    use crate::package::testing::PackageExt;
    use crate::package::Package;
    use crate::target::ConfiguredTargetLabel;
    use crate::target::TargetLabel;
    use crate::target::TargetName;

    pub trait ConfiguredTargetLabelExt {
        /// creates 'ConfiguredTargetLabel'
        fn testing_new(
            pkg: Package,
            label: TargetName,
            cfg: Configuration,
        ) -> ConfiguredTargetLabel {
            ConfiguredTargetLabel {
                target: TargetLabel { pkg, name: label },
                cfg,
                exec_cfg: None,
            }
        }
    }
    impl ConfiguredTargetLabelExt for ConfiguredTargetLabel {}

    pub trait TargetLabelExt {
        /// Simple and incorrect target label parser which can be used in tests.
        fn testing_parse(target_label: &str) -> TargetLabel {
            let (cell, cell_rel) = target_label.split_once("//").expect("no //");
            let (path, name) = cell_rel.split_once(':').expect("no :");
            let pkg = Package::testing_new(cell, path);
            let name = TargetName::unchecked_new(name);
            TargetLabel { pkg, name }
        }
    }
    impl TargetLabelExt for TargetLabel {}
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::target::TargetName;

    #[test]
    fn target_name_validation() {
        assert_eq!(
            TargetName::new("foo").unwrap(),
            TargetName(Arc::from("foo"))
        );
        assert_eq!(
            // Copied allowed symbols from above.
            // `,`, `.`, `=`, `-`, `/`, `~`, `@`, `!`, `+` and `_`
            TargetName::new("foo,.=-/~@$!+_1").unwrap(),
            TargetName(Arc::from("foo,.=-/~@$!+_1"))
        );
        assert_eq!(TargetName::new("foo bar").is_err(), true);
        assert_eq!(TargetName::new("foo?bar").is_err(), true);

        if let Err(e) = TargetName::new("target[label]") {
            let msg = format!("{:#}", e);
            assert!(msg.contains("found inner providers label when target names are expected. remove `[...]` portion of the target name from `target[label]`"), "{}", msg);
        } else {
            panic!("should have gotten an error")
        }
    }
}
