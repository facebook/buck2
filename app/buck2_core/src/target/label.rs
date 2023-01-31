/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use allocative::Allocative;
use dupe::Dupe;
use serde::Serialize;
use serde::Serializer;

use crate::configuration::Configuration;
use crate::package::PackageLabel;
use crate::target::name::TargetName;
use crate::target::name::TargetNameRef;

/// 'TargetLabel' that uniquely maps to a 'target'
/// It contains a 'Package' which is the 'Package' defined by the build fine
/// that contains this 'target', and a 'name' which is a 'TargetName'
/// representing the target name given to the particular target.
#[derive(
    Clone,
    Dupe,
    Debug,
    derive_more::Display,
    Hash,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Allocative
)]
#[display(fmt = "{}:{}", pkg, name)]
pub struct TargetLabel {
    pkg: PackageLabel,
    name: TargetName,
}

impl TargetLabel {
    #[inline]
    pub fn new(pkg: PackageLabel, name: &TargetNameRef) -> Self {
        TargetLabel {
            pkg,
            name: name.to_owned(),
        }
    }

    #[inline]
    pub fn pkg(&self) -> PackageLabel {
        self.pkg.dupe()
    }

    #[inline]
    pub fn name(&self) -> &TargetNameRef {
        self.name.as_ref()
    }

    /// Creates a 'ConfiguredTargetLabel' from ['Self'] based on the provided
    /// configuration.
    #[inline]
    pub fn configure(&self, cfg: Configuration) -> ConfiguredTargetLabel {
        ConfiguredTargetLabel {
            target: self.dupe(),
            cfg,
            exec_cfg: None,
        }
    }

    /// Like `configure`, but forces the execution configuration too.
    #[inline]
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
    #[inline]
    pub fn pkg(&self) -> PackageLabel {
        self.target.pkg.dupe()
    }

    #[inline]
    pub fn name(&self) -> &TargetNameRef {
        self.target.name()
    }

    #[inline]
    pub fn unconfigured(&self) -> &TargetLabel {
        &self.target
    }

    #[inline]
    pub fn cfg(&self) -> &Configuration {
        &self.cfg
    }

    #[inline]
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
pub trait TargetLabelMaybeConfigured: Display {}

impl TargetLabelMaybeConfigured for TargetLabel {}
impl TargetLabelMaybeConfigured for ConfiguredTargetLabel {}

pub mod testing {
    use crate::configuration::Configuration;
    use crate::package::testing::PackageExt;
    use crate::package::PackageLabel;
    use crate::target::label::ConfiguredTargetLabel;
    use crate::target::label::TargetLabel;
    use crate::target::name::TargetName;

    pub trait ConfiguredTargetLabelExt {
        /// creates 'ConfiguredTargetLabel'
        fn testing_new(
            pkg: PackageLabel,
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
            let pkg = PackageLabel::testing_new(cell, path);
            let name = TargetName::unchecked_new(name);
            TargetLabel { pkg, name }
        }
    }
    impl TargetLabelExt for TargetLabel {}
}
