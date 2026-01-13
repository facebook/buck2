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
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_util::arc_str::ThinArcStr;
use dice_futures::cancellation::CancellationContext;
use dice_futures::cancellation::CancellationObserver;
use dupe::Dupe;

use crate::paths::module::OwnedStarlarkModulePath;

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
    pub fn as_path(&self) -> buck2_error::Result<ForwardRelativePathBuf> {
        let mut path = self.to_string();

        // Just replace some characters to make a path that's a little easier to deal with in the shell.
        for c in ",(): ".chars() {
            path = path.replace(c, "_");
        }
        path = path.replace("//", "/");
        if let Some(p) = path.strip_suffix("/") {
            path = p.to_owned();
        }

        ForwardRelativePathBuf::new(path)
    }
}

impl std::fmt::Display for StarlarkEvalKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // The display result for StarlarkEvalKind is used for:
        // 1. the name of the "thread" in the starlark debugger
        // 2. the profile target when listed in targets.txt of a merged profile output
        // 3. the identifier for matching with --profile-patterns
        // 4. the output path when profiling with --profile-patterns
        match self {
            StarlarkEvalKind::Analysis(label) => write!(
                f,
                "analysis/{}/{}/{}",
                label.pkg().cell_relative_path(),
                label.name(),
                label.cfg().full_name().replace("/", "_")
            ),
            StarlarkEvalKind::Load(arc) => match &**arc {
                OwnedStarlarkModulePath::LoadFile(import_path) => {
                    write!(f, "load_bzl/{}", import_path)
                }
                OwnedStarlarkModulePath::BxlFile(bxl_file_path) => {
                    write!(f, "load_bxl/{}", bxl_file_path)
                }
                OwnedStarlarkModulePath::JsonFile(import_path) => {
                    write!(f, "load_json/{}", import_path)
                }
                OwnedStarlarkModulePath::TomlFile(import_path) => {
                    write!(f, "load_toml/{}", import_path)
                }
            },
            StarlarkEvalKind::LoadPackageFile(package_label) => {
                write!(f, "load_package/{}", package_label)
            }
            StarlarkEvalKind::LoadBuildFile(package_label) => write!(f, "load/{}", package_label),
            StarlarkEvalKind::Transition(t) => write!(f, "transition/{}", t),
            StarlarkEvalKind::AnonTarget(target) => write!(f, "anon_target/{}", target),
            StarlarkEvalKind::Bxl(bxl) => write!(f, "bxl/{}", bxl),
            StarlarkEvalKind::BxlDynamic(bxl_dynamic) => write!(f, "bxl_dynamic/{}", bxl_dynamic),
            StarlarkEvalKind::Unknown(label) => write!(f, "unknown/{}", label),
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
