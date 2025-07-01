/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::any::Any;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::package::PackageLabel;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_futures::cancellation::CancellationContext;
use buck2_futures::cancellation::CancellationObserver;
use buck2_util::arc_str::ThinArcStr;
use dupe::Dupe;

use crate::paths::module::OwnedStarlarkModulePath;
use crate::starlark_profiler::data::ProfileTarget;

pub trait DynEvalKindKey: Display + Send + Sync + Debug + Allocative + 'static {
    fn hash(&self, state: &mut dyn Hasher);
    fn eq(&self, other: &dyn DynEvalKindKey) -> bool;
    fn as_any(&self) -> &dyn Any;
}

impl<T: Display + Send + Sync + Debug + Allocative + Hash + Eq + PartialEq + Any + 'static>
    DynEvalKindKey for T
{
    fn hash(&self, mut state: &mut dyn Hasher) {
        Hash::hash(self, &mut state)
    }

    fn eq(&self, other: &dyn DynEvalKindKey) -> bool {
        match other.as_any().downcast_ref::<Self>() {
            None => false,
            Some(v) => v == self,
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Hash for dyn DynEvalKindKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        DynEvalKindKey::hash(self, state)
    }
}

impl PartialEq for dyn DynEvalKindKey {
    fn eq(&self, other: &Self) -> bool {
        DynEvalKindKey::eq(self, other)
    }
}

impl Eq for dyn DynEvalKindKey {}

/// StarlarkEvalKind is used as an identifier for a particular starlark evaluation.
///
/// It's used to selectively enable profiling and provides an identifier for the debugger.
#[derive(Debug, Clone, Dupe, Hash, Eq, PartialEq, Allocative)]
pub enum StarlarkEvalKind {
    Analysis(ConfiguredTargetLabel),
    Load(Arc<OwnedStarlarkModulePath>),
    LoadPackageFile(PackageLabel),
    LoadBuildFile(PackageLabel),
    Transition(Arc<TransitionId>),
    // These types are defined in higher crates, so we just accept dyn DynEvalKindKey here.
    AnonTarget(Arc<dyn DynEvalKindKey>),
    Bxl(Arc<dyn DynEvalKindKey>),
    BxlDynamic(Arc<dyn DynEvalKindKey>),
    Unknown(ThinArcStr),
}

impl StarlarkEvalKind {
    pub fn to_profile_target(&self) -> buck2_error::Result<ProfileTarget> {
        match self {
            StarlarkEvalKind::Analysis(label) => Ok(ProfileTarget::Analysis(label.dupe())),
            StarlarkEvalKind::LoadBuildFile(package) => Ok(ProfileTarget::Loading(package.dupe())),
            StarlarkEvalKind::Bxl(_) => Ok(ProfileTarget::Bxl),
            StarlarkEvalKind::BxlDynamic(_) => Ok(ProfileTarget::Bxl),
            _ => Ok(ProfileTarget::Unknown),
        }
    }
}

impl std::fmt::Display for StarlarkEvalKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StarlarkEvalKind::Analysis(target) => write!(f, "analysis:{}", target),
            StarlarkEvalKind::Load(module) => write!(f, "load:{}", module),
            StarlarkEvalKind::LoadPackageFile(package) => write!(f, "load_packagefile:{}", package),
            StarlarkEvalKind::LoadBuildFile(package) => write!(f, "load_buildfile:{}", package),
            StarlarkEvalKind::AnonTarget(key) => write!(f, "anon_analysis:{}", key),
            StarlarkEvalKind::Transition(id) => write!(f, "transition:{}", id),
            StarlarkEvalKind::Bxl(key) => write!(f, "bxl:{}", key),
            StarlarkEvalKind::BxlDynamic(key) => write!(f, "bxl_dynamic:{}", key),
            StarlarkEvalKind::Unknown(key) => write!(f, "generic_starlark:{}", key),
        }
    }
}

#[derive(Clone, Dupe)]
pub enum CancellationPoller<'a> {
    None,
    Context(&'a CancellationContext),
    Observer(CancellationObserver),
}

impl<'a> From<&'a CancellationContext> for CancellationPoller<'a> {
    fn from(v: &'a CancellationContext) -> Self {
        Self::Context(v)
    }
}

impl<'a> From<Option<&'a CancellationContext>> for CancellationPoller<'a> {
    fn from(v: Option<&'a CancellationContext>) -> Self {
        v.map_or(Self::None, |c| c.into())
    }
}

impl<'a> From<Option<()>> for CancellationPoller<'a> {
    fn from(_v: Option<()>) -> Self {
        Self::None
    }
}

impl<'a> From<CancellationObserver> for CancellationPoller<'a> {
    fn from(v: CancellationObserver) -> Self {
        Self::Observer(v)
    }
}
