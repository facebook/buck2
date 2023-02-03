/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use dupe::Dupe;
use fnv::FnvHasher;
use internment_tweaks::Intern;
use internment_tweaks::StaticInterner;
use once_cell::sync::Lazy;

use crate::configuration::Configuration;

#[derive(Debug, thiserror::Error)]
enum ConfigurationPairError {
    #[error("`ConfigurationPair` has unexpected `exec_cfg`")]
    HasExecCfg,
}

#[derive(Debug, Allocative, Hash, Eq, PartialEq, Ord, PartialOrd)]
struct ConfigurationPairData {
    cfg: Configuration,
    /// Usually this is None, but for toolchain deps where the exec_cfg isn't picked it is set
    exec_cfg: Option<Configuration>,
}

/// Pair of `cfg` and `exec_cfg`.
/// These two are added to `TargetLabel` to make `ConfiguredTargetLabel`.
#[derive(Debug, Clone, Dupe, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative)]
pub struct ConfigurationPair(Intern<ConfigurationPairData>);

static INTERNER: StaticInterner<ConfigurationPairData, FnvHasher> = StaticInterner::new();

impl ConfigurationPair {
    #[inline]
    pub fn new(cfg: Configuration, exec_cfg: Option<Configuration>) -> ConfigurationPair {
        ConfigurationPair(INTERNER.intern(ConfigurationPairData { cfg, exec_cfg }))
    }

    #[inline]
    pub fn cfg(&self) -> &Configuration {
        &self.0.cfg
    }

    #[inline]
    pub fn exec_cfg(&self) -> Option<&Configuration> {
        self.0.exec_cfg.as_ref()
    }

    #[inline]
    pub fn check_no_exec_cfg(&self) -> anyhow::Result<ConfigurationPairNoExec> {
        if self.exec_cfg().is_some() {
            return Err(ConfigurationPairError::HasExecCfg.into());
        }
        Ok(ConfigurationPairNoExec(self.dupe()))
    }
}

/// `ConfigurationPair` where `exec_cfg` is always `None`.
#[derive(Debug, Clone, Dupe, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative)]
pub struct ConfigurationPairNoExec(ConfigurationPair);

impl ConfigurationPairNoExec {
    #[inline]
    pub fn new(cfg: Configuration) -> ConfigurationPairNoExec {
        ConfigurationPairNoExec(ConfigurationPair::new(cfg, None))
    }

    #[inline]
    pub fn unbound() -> ConfigurationPairNoExec {
        static UNBOUND: Lazy<ConfigurationPairNoExec> =
            Lazy::new(|| ConfigurationPairNoExec::new(Configuration::unbound()));
        UNBOUND.dupe()
    }

    #[inline]
    pub fn unspecified_exec() -> Self {
        static UNSPECIFIED_EXEC: Lazy<ConfigurationPairNoExec> =
            Lazy::new(|| ConfigurationPairNoExec::new(Configuration::unspecified_exec()));
        UNSPECIFIED_EXEC.dupe()
    }

    #[inline]
    pub fn unbound_exec() -> Self {
        static UNBOUND_EXEC: Lazy<ConfigurationPairNoExec> =
            Lazy::new(|| ConfigurationPairNoExec::new(Configuration::unbound_exec()));
        UNBOUND_EXEC.dupe()
    }

    #[inline]
    pub fn unspecified() -> Self {
        static UNSPECIFIED: Lazy<ConfigurationPairNoExec> =
            Lazy::new(|| ConfigurationPairNoExec::new(Configuration::unspecified()));
        UNSPECIFIED.dupe()
    }

    #[inline]
    pub fn testing_new() -> Self {
        static TESTING_NEW: Lazy<ConfigurationPairNoExec> =
            Lazy::new(|| ConfigurationPairNoExec::new(Configuration::testing_new()));
        TESTING_NEW.dupe()
    }

    #[inline]
    pub fn cfg_pair(&self) -> &ConfigurationPair {
        &self.0
    }

    #[inline]
    pub fn cfg(&self) -> &Configuration {
        self.cfg_pair().cfg()
    }

    #[inline]
    pub fn make_toolchain(&self, exec_cfg: &ConfigurationPairNoExec) -> ConfigurationPairWithExec {
        ConfigurationPairWithExec::new(self.cfg().dupe(), exec_cfg.cfg().dupe())
    }
}

/// `ConfigurationPair` where `exec_cfg` is always `Some`.
#[derive(Debug, Clone, Dupe, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative)]
pub struct ConfigurationPairWithExec(ConfigurationPair);

impl ConfigurationPairWithExec {
    #[inline]
    pub fn new(cfg: Configuration, exec_cfg: Configuration) -> ConfigurationPairWithExec {
        ConfigurationPairWithExec(ConfigurationPair::new(cfg, Some(exec_cfg)))
    }

    #[inline]
    pub fn cfg(&self) -> &Configuration {
        self.cfg_pair().cfg()
    }

    #[inline]
    pub fn exec_cfg(&self) -> &Configuration {
        self.cfg_pair()
            .exec_cfg()
            .expect("`exec_cfg` is always `Some`")
    }

    #[inline]
    pub fn cfg_pair(&self) -> &ConfigurationPair {
        &self.0
    }
}
