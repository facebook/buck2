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

use crate::configuration::ConfigurationData;

#[derive(Debug, thiserror::Error)]
enum ConfigurationError {
    #[error("`ConfigurationPair` has unexpected `exec_cfg`")]
    HasExecCfg,
}

#[derive(Debug, Allocative, Hash, Eq, PartialEq, Ord, PartialOrd)]
struct ConfigurationPairData {
    cfg: ConfigurationData,
    /// Usually this is None, but for toolchain deps where the exec_cfg isn't picked it is set
    exec_cfg: Option<ConfigurationData>,
}

/// Pair of `cfg` and `exec_cfg`.
/// These two are added to `TargetLabel` to make `ConfiguredTargetLabel`.
#[derive(Debug, Clone, Dupe, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative)]
pub struct Configuration(Intern<ConfigurationPairData>);

static INTERNER: StaticInterner<ConfigurationPairData, FnvHasher> = StaticInterner::new();

impl Configuration {
    #[inline]
    pub fn new(cfg: ConfigurationData, exec_cfg: Option<ConfigurationData>) -> Configuration {
        Configuration(INTERNER.intern(ConfigurationPairData { cfg, exec_cfg }))
    }

    #[inline]
    pub fn cfg(&self) -> &ConfigurationData {
        &self.0.cfg
    }

    #[inline]
    pub fn exec_cfg(&self) -> Option<&ConfigurationData> {
        self.0.exec_cfg.as_ref()
    }

    #[inline]
    pub fn check_no_exec_cfg(&self) -> anyhow::Result<ConfigurationNoExec> {
        if self.exec_cfg().is_some() {
            return Err(ConfigurationError::HasExecCfg.into());
        }
        Ok(ConfigurationNoExec(self.dupe()))
    }
}

/// `ConfigurationPair` where `exec_cfg` is always `None`.
#[derive(Debug, Clone, Dupe, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative)]
pub struct ConfigurationNoExec(Configuration);

impl ConfigurationNoExec {
    #[inline]
    pub fn new(cfg: ConfigurationData) -> ConfigurationNoExec {
        ConfigurationNoExec(Configuration::new(cfg, None))
    }

    #[inline]
    pub fn unbound() -> ConfigurationNoExec {
        static UNBOUND: Lazy<ConfigurationNoExec> =
            Lazy::new(|| ConfigurationNoExec::new(ConfigurationData::unbound()));
        UNBOUND.dupe()
    }

    #[inline]
    pub fn unspecified_exec() -> Self {
        static UNSPECIFIED_EXEC: Lazy<ConfigurationNoExec> =
            Lazy::new(|| ConfigurationNoExec::new(ConfigurationData::unspecified_exec()));
        UNSPECIFIED_EXEC.dupe()
    }

    #[inline]
    pub fn unbound_exec() -> Self {
        static UNBOUND_EXEC: Lazy<ConfigurationNoExec> =
            Lazy::new(|| ConfigurationNoExec::new(ConfigurationData::unbound_exec()));
        UNBOUND_EXEC.dupe()
    }

    #[inline]
    pub fn unspecified() -> Self {
        static UNSPECIFIED: Lazy<ConfigurationNoExec> =
            Lazy::new(|| ConfigurationNoExec::new(ConfigurationData::unspecified()));
        UNSPECIFIED.dupe()
    }

    #[inline]
    pub fn testing_new() -> Self {
        static TESTING_NEW: Lazy<ConfigurationNoExec> =
            Lazy::new(|| ConfigurationNoExec::new(ConfigurationData::testing_new()));
        TESTING_NEW.dupe()
    }

    #[inline]
    pub fn cfg_pair(&self) -> &Configuration {
        &self.0
    }

    #[inline]
    pub fn cfg(&self) -> &ConfigurationData {
        self.cfg_pair().cfg()
    }

    #[inline]
    pub fn make_toolchain(&self, exec_cfg: &ConfigurationNoExec) -> ConfigurationWithExec {
        ConfigurationWithExec::new(self.cfg().dupe(), exec_cfg.cfg().dupe())
    }
}

/// `ConfigurationPair` where `exec_cfg` is always `Some`.
#[derive(Debug, Clone, Dupe, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative)]
pub struct ConfigurationWithExec(Configuration);

impl ConfigurationWithExec {
    #[inline]
    pub fn new(cfg: ConfigurationData, exec_cfg: ConfigurationData) -> ConfigurationWithExec {
        ConfigurationWithExec(Configuration::new(cfg, Some(exec_cfg)))
    }

    #[inline]
    pub fn cfg(&self) -> &ConfigurationData {
        self.cfg_pair().cfg()
    }

    #[inline]
    pub fn exec_cfg(&self) -> &ConfigurationData {
        self.cfg_pair()
            .exec_cfg()
            .expect("`exec_cfg` is always `Some`")
    }

    #[inline]
    pub fn cfg_pair(&self) -> &Configuration {
        &self.0
    }
}
